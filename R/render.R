#' Render Markdown to an output format
#'
#' Render Markdown to an output format via the \pkg{commonmark} package. The
#' function `mark_html()` is a shorthand of `mark(format = 'html')`, and
#' `mark_latex()` is a shorthand of `mark(format = 'latex')`.
#' @param file Path to an input file. If not provided, it is presumed that the
#'   `text` argument will be used instead. This argument can also take a
#'   character vector of Markdown text directly. To avoid ambiguity in the
#'   latter case, a single character string input will be treated as a file if
#'   the file exists. If a string should be treated as Markdown text when it
#'   happens to be a file path, wrap it in [I()].
#' @param output Output file path. If not character, the results will be
#'   returned as a character vector. If not specified and the input is a file
#'   path, the output file path will have the same base name as the input file,
#'   with an extension corresponding to the `format` argument, e.g.,
#'   `mark('foo.md', format = 'latex')` will generate an output file
#'   \file{foo.tex} by default.
#' @param text A character vector of the Markdown text. By default, it is read
#'   from `file`.
#' @param format An output format supported by \pkg{commonmark}, e.g., `'html'`,
#'   `'man'`, and `'text'`, etc. See the
#'   [`markdown_*()`][commonmark::commonmark] renderers in \pkg{commonmark}.
#' @param options Options to be passed to the renderer. See [markdown_options()]
#'   for details. This argument can take either a character vector of the form
#'   `"+option1 option2-option3"` (use `+` or a space to enable an option, and
#'   `-` to disable an option), or a list of the form `list(option1 = value1,
#'   option2 = value2, ...)`. A string `"+option1"` is equivalent to
#'   `list(option1 = TRUE)`, and `"-option2"` means `list(option2 = FALSE)`.
#'   Options that do not take logical values must be specified via a list, e.g.,
#'   `list(width = 30)`.
#' @param meta A named list of metadata. Elements in the metadata will be used
#'   to fill out the template by their names and values, e.g., `list(title =
#'   ...)` will replace the `$title$` variable in the template. See the Section
#'   \dQuote{YAML metadata} in the vignette `vignette('intro', package =
#'   'litedown')` for supported variables.
#' @return Invisible `NULL` when output is to a file, otherwise a character
#'   vector of the rendered output.
#' @seealso The spec of GitHub Flavored Markdown:
#'   <https://github.github.com/gfm/>
#' @import utils
#' @export
#' @examples
#' library(litedown)
#' mark(c('Hello _World_!', '', 'Welcome to **litedown**.'))
#' # a few corner cases
#' mark(character(0))
#' mark('')
#' # if input happens to be a file path but should be treated as text, use I()
#' mark(I('This is *not* a file.md'))
#' # that's equivalent to
#' mark(text = 'This is *not* a file.md')
mark = function(
  file = NULL, output = NULL, text = NULL, format = c('html', 'latex'),
  options = NULL, meta = list()
) {
  if (is.null(text)) {
    if (!is.character(file)) stop("Either 'file' or 'text' must be provided.")
    text = if (is_file(file)) xfun::read_utf8(file) else file
  }
  text = xfun::split_lines(text)

  part = xfun::yaml_body(text); yaml = part$yaml; text = part$body
  format = format[1]
  # title/author/date can be provided as top-level YAML options
  meta = merge_list(
    yaml[intersect(names(yaml), c('title', 'author', 'date'))],
    yaml_field(yaml, format),
    meta
  )
  meta = normalize_meta(meta)

  if (missing(output) && is_file(file)) output = auto_output(file, format)

  render_fun = tryCatch(
    getFromNamespace(paste0('markdown_', tolower(format)), 'commonmark'),
    error = function(e) {
      stop("Output format '", format, "' is not supported in commonmark.")
    }
  )

  options = merge_list(yaml_field(yaml, format, 'options'), option2list(options))
  options = normalize_options(options, format)
  options$extensions = intersect(
    names(Filter(isTRUE, options)), commonmark::list_extensions()
  )

  # determine the template: first check the `template` value in the output
  # format litedown::(html|latex)_format in YAML
  template = yaml_field(yaml, format, 'template')
  # if not set there, check global option; if not set, disable template if no
  # YAML was provided (i.e., generate a fragment)
  if (is.null(template)) template = get_option(
    sprintf('litedown.%s.template', format), length(yaml) > 0
  )
  # template = FALSE means no template; other values mean the default template
  if (!is.character(template)) template = if (!isFALSE(template))
    pkg_file('resources', sprintf('litedown.%s', format))

  render_args = options[intersect(names(formals(render_fun)), names(options))]
  render = function(x, clean = FALSE) {
    if (length(x) == 0) return(x)
    res = do.call(render_fun, c(list(text = x), render_args))
    if (clean) res = gsub('^<p[^>]*>|(</p>)?\n$', '', res)
    res
  }

  if (isTRUE(options[['smartypants']])) text = smartypants(text)

  # test if a feature needs to be enabled
  test_feature = function(name, pattern) {
    isTRUE(options[[name]]) && format %in% c('html', 'latex') &&
      length(grep(pattern, text, perl = TRUE))
  }

  # protect $ $ and $$ $$ math expressions for html/latex output
  if (has_math <- test_feature('latex_math', '[$]')) {
    id = id_string(text); maths = NULL
    text = xfun::protect_math(text, id)
    # temporarily replace math expressions with tokens and restore them later;
    # no need to do this for html output because we need special HTML characters
    # like &<> in math expressions to be converted to entities, but shouldn't
    # convert them for latex output
    if (format == 'latex') {
      text = one_string(text)
      text = match_replace(text, sprintf('`%s.{3,}?%s`', id, id), function(x) {
        maths <<- c(maths, gsub(sprintf('`%s|%s`', id, id), '', x))
        # replace math with !id-n-id! where n is the index of the math
        sprintf('!%s-%d-%s!', id, length(maths) + seq_along(x), id)
      })
      text = xfun::split_lines(text)
    }
  }

  p = NULL  # indices of prose
  find_prose = function() if (is.null(p)) p <<- xfun::prose_index(text)
  # superscript and subscript; for now, we allow only characters alnum|*|(|) for
  # script text but can consider changing this rule upon users' request
  r2 = '(?<!`)\\^([[:alnum:]*()]+?)\\^(?!`)'
  if (has_sup <- test_feature('superscript', r2)) {
    id2 = id_string(text)
    find_prose()
    text[p] = match_replace(text[p], r2, function(x) {
      # place superscripts inside !id...id!
      x = gsub('^\\^|\\^$', id2, x)
      sprintf('!%s!', x)
    })
  }
  r3 = '(?<![~`[:space:]])~([[:alnum:]*()]+?)~(?!`)'
  if (has_sub <- test_feature('subscript', r3)) {
    id3 = id_string(text)
    find_prose()
    text[p]= match_replace(text[p], r3, function(x) {
      # place subscripts inside !id...id!
      x = gsub('^~|~$', id3, x)
      sprintf('!%s!', x)
    })
  }
  # disallow single tilde for <del> (I think it is an awful idea in GFM's
  # strikethrough extension to allow both single and double tilde for <del>)
  find_prose()
  text[p] = match_replace(text[p], r3, function(x) {
    gsub('^~|~$', '\\\\~', x)
  })
  # add line breaks before/after fenced Div's to wrap ::: tokens into separate
  # paragraphs or code blocks
  text[p] = sub('^([ >]*:::+ )([^ {]+)$', '\\1{.\\2}', text[p]) # ::: foo -> ::: {.foo}
  text[p] = sub(
    '^([ >]*)((:::+)( \\{.+\\})?)$',
    if (format == 'latex') '\\1\n\\1```\n\\1\\2 \\3\n\\1```\n\\1' else '\\1\n\\1\\2\n\\1',
    text[p]
  )

  id4 = id_string(text)
  if (format == 'latex') {
    # put info string inside code blocks so the info won't be lost, e.g., ```r -> ```\nr
    text = gsub(
      '^([> ]*)(```+)([^`].*)$', sprintf('\\1\\2\n\\1%s\\3%s', id4, id4), text
    )
  } else if (format == 'html' && length(p) < length(text)) {
    # hide spaces so that attributes won't be dropped: {.lang foo} -> {.lang!id!foo}
    r4 = '^([> ]*```+)(\\{.+})\\s*$'
    text = match_replace(text, r4, function(x) {
      x1 = sub(r4, '\\1', x)
      x2 = sub(r4, '\\2', x)
      x2 = gsub(' ', id4, x2, fixed = TRUE)
      paste0(x1, x2)
    })
  }

  ret = render(text)
  ret = move_attrs(ret, format)  # apply attributes of the form {attr="value"}

  if (format == 'html') {
    if (has_math) {
      ret = gsub(sprintf('<code>%s(.{5,}?)%s</code>', id, id), '\\1', ret)
      # `\(math\)` may fail to render to <code>\(math\)</code> when backticks
      # are inside HTML tags, e.g., commonmark::markdown_html('<p>`a`</p>')
      ret = gsub(sprintf('`%s\\\\\\((.+?)\\\\\\)%s`', id, id), '$\\1$', ret)
    }
    if (has_sup)
      ret = gsub(sprintf('!%s(.+?)%s!', id2, id2), '<sup>\\1</sup>', ret)
    if (has_sub)
      ret = gsub(sprintf('!%s(.+?)%s!', id3, id3), '<sub>\\1</sub>', ret)
    r4 = '<pre><code class="language-\\{=([^}]+)}">(.+?)</code></pre>\n'
    ret = match_replace(ret, r4, function(x) {
      lang = gsub(r4, '\\1', x)
      code = gsub(r4, '\\2', x)
      # restore raw html content from ```{=html}
      i1 = lang == 'html'
      x[i1] = restore_html(code[i1])
      # possible math environments
      i2 = (lang %in% c('tex', 'latex')) &
        grepl('^\\\\begin\\{[a-zA-Z*]+\\}.+\\\\end\\{[a-zA-Z*]+\\}\n$', code)
      x[i2] = sprintf('<p>\n%s</p>\n', code[i2])
      # discard other types of raw content blocks
      x[!(i1 | i2)] = ''
      x
    }, perl = FALSE)  # for perl = TRUE, we'd need (?s) before (.+?)
    r4 = '(<pre><code class="language-)\\{([^"]+)}">'
    # deal with ```{.class1 .class2 attrs}, which is not supported by commonmark
    ret = convert_attrs(ret, r4, '\\2', function(r, z, z2) {
      z1 = sub(r, '\\1', z)
      # make sure `class` is the first attribute
      z2 = gsub('^(.+?)( +)(class="[^"]+")(.*)$', '\\3 \\1\\4', z2)
      i = grepl('^class="', z2)
      z2 = ifelse(i, sub('^class="', '', z2), paste0('"', z2))
      paste0(z1, z2, '>')
    }, 'html', function(z2) gsub(id4, ' ', restore_html(z2)))
    # some code blocks with "attributes" are verbatim ones
    ret = match_replace(ret, '```+\\{.+}', function(x) gsub(id4, ' ', x, fixed = TRUE))
    # auto identifiers
    if (isTRUE(options[['auto_identifiers']])) ret = auto_identifier(ret)
    # number sections
    if (isTRUE(options[['number_sections']])) ret = number_sections(ret)
    # build table of contents
    ret = add_toc(ret, options)
  } else if (format == 'latex') {
    ret = render_footnotes(ret)  # render [^n] footnotes
    if (has_math) {
      m = gregexpr(sprintf('!%s-(\\d+)-%s!', id, id), ret)
      regmatches(ret, m) = lapply(regmatches(ret, m), function(x) {
        if (length(maths) != length(x)) warning(
          'LaTeX math expressions cannot be restored correctly (expected ',
          length(maths), ' expressions but found ', length(x), ' in the output).'
        )
        maths
      })
    }
    if (has_sup)
      ret = gsub(sprintf('!%s(.+?)%s!', id2, id2), '\\\\textsuperscript{\\1}', ret)
    if (has_sub)
      ret = gsub(sprintf('!%s(.+?)%s!', id3, id3), '\\\\textsubscript{\\1}', ret)
    r4 = sprintf(
      '(\\\\begin\\{verbatim}\n)%s(.+?)%s\n(.*?\n)(\\\\end\\{verbatim}\n)', id4, id4
    )
    ret = match_replace(ret, r4, function(x) {
      info = gsub(r4, '\\2', x)
      info = gsub('^\\{|}$', '', info)
      i = info %in% c('=latex', '=tex')
      x[i] = gsub(r4, '\\3', x[i])  # restore raw ```{=latex} content
      i = !i & grepl('^=', info)
      x[i] = ''  # discard other raw content
      # TODO: support code highlighting for latex (listings or highr::hi_latex)
      x = gsub(r4, '\\1\\3\\4', x)
      x
    }, perl = FALSE)
    # fix horizontal rules from --- (\linethickness doesn't work)
    ret = gsub('{\\linethickness}', '{1pt}', ret, fixed = TRUE)
    ret = redefine_level(ret, options[['top_level']])
    if (isTRUE(options[['toc']])) ret = paste0('\\tableofcontents\n', ret)
  }

  meta$body = ret
  # convert some meta variables in case they use Markdown syntax
  for (i in c('title', 'author', 'date')) meta[[i]] = render(meta[[i]], clean = TRUE)
  # use the template (if provided) to create a standalone document
  if (format %in% c('html', 'latex') && is.character(template)) {
    # add HTML dependencies to `include-headers` if found
    meta = add_html_deps(meta, output, 'local' %in% options[['embed_resources']])
    ret = build_output(format, options, template, meta)
  }

  if (format == 'html') {
    ret = xfun::in_dir(
      if (is_file(file)) dirname(file) else '.',
      embed_resources(ret, options[['embed_resources']])
    )
    ret = clean_html(ret)
  } else if (format == 'latex') {
    # remove \title and \maketitle if title is empty
    if (grepl('\n\\title{}\n', ret, fixed = TRUE))
      ret = gsub('\n(\\\\title\\{}|\\\\maketitle)\n', '\n', ret)
  }

  if (is.character(output)) xfun::write_utf8(ret, output) else ret
}

#' @rdname mark
#' @param ... Arguments to be passed to `mark()`.
#' @export
#' @examples
#'
#' mark_html('Hello _World_!')
#' # write HTML to an output file
#' mark_html('_Hello_, **World**!', output = tempfile())
mark_html = function(...) mark(..., format = 'html')

#' @export
#' @rdname mark
#' @examples
#'
#' mark_latex('Hello _World_!')
mark_latex = function(...) mark(..., format = 'latex')

# insert body and meta variables into a template
build_output = function(format, options, template, meta) {
  tpl = one_string(template, test = TRUE)
  if (format == 'html') {
    b = meta$body
    set_meta = function(name, value) {
      if (!name %in% names(meta)) meta[[name]] <<- value
    }
    set_meta('title', first_heading(b))
    set_meta('css', 'default')
    meta = set_math(meta, options, b)
    meta = set_highlight(meta, options, b)
    # special handling for css/js "files" that have no extensions
    for (i in c('css', 'js')) meta[[i]] = resolve_files(meta[[i]], i)
  }
  # find all variables in the template
  vars = unlist(regmatches(tpl, gregexpr('[$][-[:alnum:]]+[$]', tpl)))
  # insert $body$ at last in case the body contain any $variables$ accidentally
  if (!is.na(i <- match('$body$', vars))) vars = c(vars[-i], vars[i])
  for (v in vars) {
    tpl = sub_var(tpl, v, meta[[gsub('[$]', '', v)]])
  }
  tpl
}

#' Markdown rendering options
#'
#' A list of all options to control Markdown rendering. Options that are enabled
#' by default are marked by a `+` prefix, and those disabled by default are
#' marked by `-`.
#'
#' See `vignette('intro', package = 'litedown')` for the full list of options
#' and their documentation.
#' @return A character vector of all available options.
#' @export
#' @examples
#' # all available options
#' litedown::markdown_options()
#'
#' @example inst/examples/render-options.R
markdown_options = function() {
  # options enabled by default
  x1 = c(
    'smart', 'smartypants', 'embed_resources', 'js_math', 'js_highlight',
    'superscript', 'subscript', 'latex_math', 'auto_identifiers',
    setdiff(commonmark::list_extensions(), 'tagfilter')
  )
  # options disabled by default
  x2 = c('toc', 'hardbreaks', 'tagfilter', 'number_sections')
  sort(c(paste0('+', x1), paste0('-', x2)))
}

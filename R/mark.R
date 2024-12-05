#' Render Markdown, R Markdown, and R scripts
#'
#' The function `mark()` renders Markdown to an output format via the
#' \pkg{commonmark} package.
#' @param input A character vector to provide the input file path or text. If
#'   not provided, the `text` argument must be provided instead. The `input`
#'   vector will be treated as a file path if it is a single string, and points
#'   to an existing file or has a filename extension. In other cases, the vector
#'   will be treated as the `text` argument input. To avoid ambiguity, if a
#'   string should be treated as `text` input when it happens to be an existing
#'   file path or has an extension, wrap it in [I()], or simply use the `text`
#'   argument instead.
#' @param output An output file path or a filename extension (e.g., `.html`,
#'   `.tex`, `.xml`, `.man`, `.markdown`, or `.txt`). In the latter case, the
#'   output file path will use the extension on the same base filename as the
#'   input file if the `input` is a file. If `output` is not character (e.g.,
#'   `NA`), the results will be returned as a character vector instead of being
#'   written to a file. If `output` is `NULL` or an extension, and the input is
#'   a file path, the output file path will have the same base name as the input
#'   file, with an extension corresponding to the output format. The output
#'   format is retrieved from the first value in the `output` field of the YAML
#'   metadata of the `input` (e.g., `html` will generate HTML
#'   output). The `output` argument can also take an output format name
#'   (possible values are `html`, `latex`, `xml`, `man`, `commonmark`, and
#'   `text`). If no output format is detected or provided, the default is HTML.
#' @param text A character vector as the text input. By default, it is read from
#'   the `input` file if provided.
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
#'   \dQuote{YAML metadata} [in the
#'   documentation](https://yihui.org/litedown/#sec:yaml-metadata) for supported
#'   variables.
#' @return The output file path if output is written to a file, otherwise a
#'   character vector of the rendered output (wrapped in [xfun::raw_string()]
#'   for clearer printing).
#' @seealso The spec of GitHub Flavored Markdown:
#'   <https://github.github.com/gfm/>
#' @import utils
#' @export
#' @examples
#'
#' mark(c('Hello _World_!', '', 'Welcome to **litedown**.'))
#' # if input appears to be a file path but should be treated as text, use I()
#' mark(I('This is *not* a file.md'))
#' # that's equivalent to
#' mark(text = 'This is *not* a file.md')
#'
#' # output to a file
#' (mark('_Hello_, **World**!', output = tempfile()))
#'
#' # convert to other formats
#' mark('Hello _World_!', '.tex')
#' mark('Hello _**`World`**_!', 'xml')
#' mark('Hello _**`World`**_!', 'text')
mark = function(input, output = NULL, text = NULL, options = NULL, meta = list()) {
  text = read_input(input, text); input = attr(text, 'input')
  part = yaml_body(text)
  yaml = part$yaml; yaml2 = yaml_text(part, text)  # unparsed YAML
  text = part$body

  full = is_output_full(output)
  format = detect_format(output, yaml)
  output = auto_output(input, output, format)
  out_dir = dirname(output_path(input, output) %||% '.')

  # title/author/date can be provided as top-level YAML options
  meta = merge_list(
    get_option('meta', format),
    yaml[intersect(names(yaml), top_meta)],
    yaml_field(yaml, format),
    list(generator = I(paste('litedown', packageVersion('litedown')))),
    meta
  )
  meta = normalize_meta(meta)

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

  # whether to write YAML metadata to output
  keep_yaml = isTRUE(options[['keep_yaml']])

  # if keep_yaml or format is not html/latex, don't use template; otherwise
  # check the `template` value in litedown::(html|latex)_format in YAML
  template = if (keep_yaml || !format %in% c('html', 'latex')) FALSE else
    yaml_field(yaml, format, 'template')
  # if not set there, check global option; if not set, disable template if no
  # YAML was provided (i.e., generate a fragment)
  if (is.null(template))
    template = get_option('template', format, full || 'yaml' %in% names(part))
  # template = FALSE means no template; other values mean the default template
  if (!is.character(template)) template = if (!isFALSE(template))
    pkg_file('resources', sprintf('litedown.%s', format))

  render_args = options[intersect(names(formals(render_fun)), names(options))]
  render = function(x, clean = FALSE) {
    if (length(x) == 0) return(x)
    res = do.call(render_fun, c(list(text = x), render_args))
    if (clean) res = sans_p(res)
    I(res)
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
    if (has_math <- any(grepl(paste0('`', id), text, fixed = TRUE))) {
      # temporarily replace math expressions with tokens so render() won't seem
      # them (to avoid issues like #33) and restore them later
      text = one_string(text)
      text = match_replace(text, sprintf('`%s(?s).{3,}?%s`', id, id), function(x) {
        n0 = length(maths)
        maths <<- c(maths, gsub(sprintf('`%s|%s`', id, id), '', x))
        # replace math with !id-n-id! where n is the index of the math
        sprintf('!%s-%d-%s!', id, n0 + seq_along(x), id)
      })
      if (format == 'html') maths = xfun::html_escape(maths)
      text = split_lines(text)
    }
  }

  p = NULL  # indices of prose
  find_prose = function() if (is.null(p)) p <<- prose_index(text)
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
  # TODO: remove this after commonmark > 1.9.2 is on CRAN
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
    '^([ >]*)((:::+)( \\{.*\\})?)$',
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
    r4 = '^([> ]*```+\\s*)(\\{.+})\\s*$'
    text = match_replace(text, r4, function(x) {
      x1 = sub(r4, '\\1', x)
      x2 = sub(r4, '\\2', x)
      x2 = gsub(' ', id4, x2, fixed = TRUE)
      paste0(x1, x2)
    })
  }

  # turn @ref into [@ref](#ref) and resolve cross-references later in JS; for
  # latex output, turn @ref to \ref{}
  r_ref = '(([a-z]+)[-:][-_[:alnum:]]+)'  # must start with letters followed by - or :
  r5 = paste0('(^|(?<=\\s|\\())@', r_ref, '(?!\\])')
  if (test_feature('cross_refs', r5)) {
    text[p] = match_replace(text[p], r5, function(x) {
      sprintf('[%s](%s)', x, sub('^@', '#', x))
    })
  }

  ret = render(text)
  ret = move_attrs(ret, format)  # apply attributes of the form {attr="value"}

  if (has_math) ret = match_replace(ret, sprintf('!%s-\\d+-%s!', id, id), function(x) {
    if (length(maths) != length(x)) warning(
      'LaTeX math expressions cannot be restored correctly (expected ',
      length(maths), ' expression(s) but found ', length(x), ' in the output).'
    )
    maths
  })

  if (format == 'html') {
    # don't disable check boxes
    ret = gsub('(<li><input type="checkbox" [^>]*?)disabled="" (/>)', '\\1\\2', ret)
    # replace <a> with <span> if href is empty but other attrs exist, so we have
    # a way to create SPANs with attributes, e.g., [text](){.foo} -> <span
    # class="foo"></span>
    ret = gsub('<a href="" ([^>]+>.*?</)a>', '<span \\1span>', ret)
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
      if (any(i2)) {
        x[i2] = sprintf('<p>\n%s</p>\n', code[i2])
        has_math <<- TRUE
      }
      # discard other types of raw content blocks
      x[!(i1 | i2)] = ''
      x
    }, perl = FALSE)  # for perl = TRUE, we'd need (?s) before (.+?)
    # support mermaid
    r_mmd = '<pre><code class="language-mermaid">(.*?)</code></pre>'
    if (length(grep(r_mmd, ret))) {
      ret = gsub(r_mmd, '<pre class="mermaid">\\1</pre>', ret)
      # add the js asset automatically if not detected
      if (length(grep('mermaid', meta[['js']])) == 0) meta = add_meta(
        meta, c(js = '@npm/mermaid/dist/mermaid.min.js')
      )
    }
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
    ret = match_replace(ret, '```+\\s*\\{.+}', function(x) gsub(id4, ' ', x, fixed = TRUE))
    # remove empty table header
    ret = gsub('<thead>\n<tr>\n(<th[^>]*></th>\n)+</tr>\n</thead>\n', '', ret)
    # table caption: a paragraph that starts with 'Table: ' or ': ' after </table>
    ret = gsub(
      '(<table>)(?s)(.+?</table>)\n<p>(Table)?: (?s)(.+?)</p>',
      '\\1\n<caption>\\4</caption>\\2', ret, perl = TRUE
    )
    # auto identifiers
    if (isTRUE(options[['auto_identifiers']])) ret = auto_identifier(ret)
    # number sections
    if (isTRUE(options[['number_sections']])) ret = number_sections(ret)
    # build table of contents
    ret = add_toc(ret, options)
    # add js/css for math
    if (!has_math) has_math = length(ret) &&
      grepl('$$</p>', ret, fixed = TRUE)  # math may be from pkg_manual()'s HTML
    is_katex = TRUE
    if (has_math && length(js_math <- js_options(options[['js_math']], 'katex'))) {
      is_katex = js_math$package == 'katex'
      meta = set_math(meta, js_math, is_katex)
    }
    # number figures and tables, etc.
    ret = number_refs(ret, r_ref, is_katex)
  } else if (format == 'latex') {
    ret = render_footnotes(ret)  # render [^n] footnotes
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

  pkg_cite = yaml_field(yaml, format, 'citation_package')
  if (length(pkg_cite) != 1) pkg_cite = 'natbib'
  bib = yaml[['bibliography']]
  if (length(bib) == 1 && grepl(',', bib)) bib = strsplit(bib, ',\\s*')[[1]]
  # add [@citation] (.bib files are assumed to be under output dir)
  if (length(bib)) {
    ret = in_dir(out_dir, add_citation(ret, bib, format))
    if (format == 'latex') meta = bib_meta(meta, bib, pkg_cite)
  }

  # convert some meta variables in case they use Markdown syntax
  if (is.character(template)) for (i in top_meta) if (meta_len <- length(meta[[i]])) {
    # if author is of length > 1, render them individually
    m_author = i == 'author' && meta_len > 1
    meta[[i]] = if (m_author) uapply(meta[[i]], render) else {
      render(meta[[i]], clean = i != 'abstract')
    }
    # also provide *_ version of top-level meta variables, containing tags/commands
    meta[[paste0(i, '_')]] = I(if (format == 'html') {
      tag = tag_meta[i]
      sprintf(
        '<div class="%s">%s</div>', i, if (tag == '') meta[[i]] else {
          one_string(sprintf('<%s>%s</%s>', tag, meta[[i]], tag))
        }
      )
    } else if (format == 'latex') {
      sprintf(cmd_meta[i], if (m_author) one_string(meta[[i]], ' \\and ') else meta[[i]])
    })
  }

  # cross references (\ref or clever \cref)
  clever = isTRUE(options[['cleveref']])
  if (format == 'latex') ret = latex_refs(ret, r_ref, clever) else clever = FALSE

  meta$body = ret

  # use the template (if provided) to create a standalone document
  if (is.character(template)) {
    ret = build_output(
      format, options, template, meta, test = c(if (length(input)) dirname(input), '.')
    )
    # load the cleveref package if not loaded
    if (clever && !any(grepl('\\\\usepackage.*\\{cleveref\\}', ret, perl = TRUE)))
      ret = sub('(?=\\\\begin\\{document\\})', '\\\\usepackage{cleveref}\n', ret, perl = TRUE)
  }

  if (format == 'html') {
    ret = in_dir(out_dir, embed_resources(ret, options))
    ret = clean_html(ret)
  } else if (format == 'latex') {
    # remove \maketitle if \title is absent
    if (!grepl('\n\\title{', ret, fixed = TRUE))
      ret = gsub('\n\\maketitle\n', '\n', ret, fixed = TRUE)
  }

  if (keep_yaml) ret = one_string(c(yaml2, '', ret))

  ret = sub('\n$', '', ret)
  if (is_output_file(output)) {
    # build PDF for LaTeX output when the output file is .pdf
    is_pdf = FALSE
    if (format == 'latex') {
      latex_engine = yaml_field(yaml, format, 'latex_engine')
      if (is.character(latex_engine) || file_ext(output) == 'pdf') {
        is_pdf = TRUE
        tex = with_ext(output, '.tex')
        if (!isTRUE(yaml_field(yaml, format, 'keep_tex')))
          on.exit(file.remove(tex), add = TRUE)
        write_utf8(ret, tex)
        output = tinytex::latexmk(
          tex, latex_engine %||% 'xelatex',
          if (pkg_cite == 'biblatex') 'biber' else 'bibtex'
        )
      }
    }
    # for RStudio to capture the output path when previewing the output
    if (is_rmd_preview()) message('\nOutput created: ', output)
    if (is_pdf) invisible(output) else write_utf8(ret, output)
  } else raw_string(ret)
}

# insert body and meta variables into a template
build_output = function(format, options, template, meta, ...) {
  tpl = one_string(template, ...)
  if (format == 'html') {
    b = meta$body
    set_meta = function(name, value) {
      if (!name %in% names(meta)) meta[[name]] <<- value
    }
    set_meta('css', 'default')
    set_meta('plain-title', I(str_trim(commonmark::markdown_text(meta[['title']]))))
    meta = set_highlight(meta, options, b)
    # if the class .line-numbers is present, add js/css for line numbers
    if (any(grepl('<code class="[^"]*line-numbers', b))) for (i in c('css', 'js')) {
      meta[[i]] = c(meta[[i]], '@code-line-numbers')
    }
    # special handling for css/js "files" that have no extensions
    for (i in c('css', 'js')) meta[[i]] = resolve_files(meta[[i]], i)
  }
  sub_vars(tpl, meta, ...)
}

# substitute all variables in template with their values
sub_vars = function(tpl, meta, ...) {
  # find all variables in the template
  vars = unlist(match_full(tpl, '[$][-_[:alnum:]]+[$]'))
  # insert $body$ at last in case the body contain any $variables$ accidentally
  if (!is.na(i <- match('$body$', vars))) vars = c(vars[-i], vars[i])
  for (v in vars) {
    tpl = sub_var(tpl, v, meta[[gsub('[$]', '', v)]], ...)
  }
  tpl
}

top_meta = c('title', 'subtitle', 'author', 'date', 'abstract')
tag_meta = c('h1', 'h2', 'h2', 'h3', '')
names(tag_meta) = top_meta
cmd_meta = c(sprintf('\\%s{%%s}', top_meta[-5]), '\\begin{abstract}\n%s\\end{abstract}')
names(cmd_meta) = top_meta

yaml_text = function(part, text) if (length(l <- part$lines) == 2) text[l[1]:l[2]]

#' Markdown rendering options
#'
#' A list of all options to control Markdown rendering. Options that are enabled
#' by default are marked by a `+` prefix, and those disabled by default are
#' marked by `-`.
#'
#' See <https://yihui.org/litedown/#sec:markdown-options> for the full list of
#' options and their documentation.
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
    'smart', 'embed_resources', 'embed_cleanup', 'js_math', 'js_highlight',
    'superscript', 'subscript', 'latex_math', 'auto_identifiers', 'cross_refs',
    setdiff(commonmark::list_extensions(), 'tagfilter')
  )
  # options disabled by default
  x2 = c('toc', 'hardbreaks', 'tagfilter', 'number_sections', 'cleveref', 'smartypants')
  sort(c(paste0('+', x1), paste0('-', x2)))
}

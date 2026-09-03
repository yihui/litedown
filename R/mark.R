#' Render Markdown, R Markdown, and R scripts
#'
#' The function `mark()` renders Markdown to an output format via the bundled
#' 'cmark-gfm' library.
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
#'   metadata of the `input` (e.g., `html` will generate HTML output). The
#'   `output` argument can also take an output format name (possible values are
#'   `html`, `latex`, `xml`, `man`, `commonmark`, and `text`). If no output
#'   format is detected or provided, the default is HTML.
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
#'   documentation](https://pkg.yihui.org/litedown/book/#sec:yaml-metadata) for supported
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
    get(paste0('markdown_', tolower(format)), envir = asNamespace('litedown')),
    error = function(e) {
      stop("Output format '", format, "' is not supported.")
    }
  )

  options = merge_list(yaml_field(yaml, format, 'options'), option2list(options))
  options = normalize_options(options, format)
  options$extensions = intersect(
    names(Filter(isTRUE, options)), list_extensions()
  )
  # the 'latex_math' option enables the C 'math' extension ($...$, $$...$$, and
  # \begin{}...\end{} environments) for html/latex output
  if (isTRUE(options[['latex_math']]) && format %in% c('html', 'latex'))
    options$extensions = union(options$extensions, 'math')
  # raw content blocks (```{=html}/```{=latex}/```{=tex}) are handled by the C
  # 'rawblock' extension for html/latex output; it is always on (raw blocks are
  # not an optional feature), so it is not exposed as a markdown_options() toggle
  if (format %in% c('html', 'latex'))
    options$extensions = union(options$extensions, 'rawblock')
  # Pandoc-style code block attributes (```{.class #id key="val"}) are rendered
  # by the C 'attributes' extension for html output; it only sets an HTML render
  # func (LaTeX/other formats ignore the info string via cmark's built-in code
  # rendering, matching the old behavior), and is always on
  if (format == 'html')
    options$extensions = union(options$extensions, 'attributes')

  # build PDF for LaTeX output when the output file is .pdf or latex_engine is specified
  is_pdf = is_output_file(output) && format == 'latex' &&
    (is.character(latex_engine <- yaml_field(yaml, format, 'latex_engine')) ||
       file_ext(output) == 'pdf')

  # whether to write YAML metadata to output
  keep_yaml = isTRUE(options[['keep_yaml']])

  # if keep_yaml or format is not html/latex, don't use template; otherwise
  # check the `template` value in litedown::(html|latex)_format in YAML
  template = if (keep_yaml || !format %in% c('html', 'latex')) FALSE else
    yaml_field(yaml, format, 'template')
  # if not set there, check global option; if not set, disable template if no
  # YAML was provided (i.e., generate a fragment)
  if (is.null(template))
    template = get_option('template', format, full || 'yaml' %in% names(part) || is_pdf)
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

  # Whether any LaTeX math is present, to decide whether to load KaTeX/MathJax.
  # We detect this from the *rendered* output (below) rather than the source,
  # because a bare `$` in the source may just be a dollar sign, inline code
  # (`$x$`), or math that has been disabled (-latex_math); detecting on the
  # source would load a math library unnecessarily. See the html branch below.
  has_math = FALSE

  p = prose_index(text)  # indices of prose
  # add line breaks before/after fenced Div's to wrap ::: tokens into separate
  # paragraphs or code blocks
  text[p] = sub('^([ >]*:::+ )([^ {]+)$', '\\1{.\\2}', text[p]) # ::: foo -> ::: {.foo}
  text[p] = sub(
    '^([ >]*)((:::+)( \\{.*\\})?)$',
    if (format == 'latex') '\\1\n\\1```\n\\1\\2 \\3\n\\1```\n\\1' else '\\1\n\\1\\2\n\\1',
    text[p]
  )

  if (format == 'latex') {
    id4 = id_string(text)
    # put info string inside code blocks so the info won't be lost, e.g., ```r -> ```\nr;
    # skip raw content blocks (```{=html}/```{=latex}/```{=tex}), whose info
    # string must reach the C 'rawblock' extension intact
    text = gsub(
      '^([> ]*)(```+)(?! *\\{=)([^`].*)$', sprintf('\\1\\2\n\\1%s\\3%s', id4, id4),
      text, perl = TRUE
    )
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

  has_mermaid = FALSE

  if (format == 'html') {
    # replace <a> with <span> if href is empty but other attrs exist, so we have
    # a way to create SPANs with attributes, e.g., [text](){.foo} -> <span
    # class="foo"></span>
    ret = gsub('<a href="" ([^>]+>.*?</)a>', '<span \\1span>', ret)
    # support mermaid
    r_mmd = '<pre><code class="language-mermaid">(.*?)</code></pre>'
    if (has_mermaid <- length(grep(r_mmd, ret))) {
      ret = gsub(r_mmd, '<pre class="mermaid">\\1</pre>', ret)
    }
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
    # a raw LaTeX/TeX math environment (```{=latex} \begin{...} ... ```) is
    # rendered as math in HTML by the C 'rawblock' extension regardless of the
    # 'latex_math' option, so it must load a math library even when math is off.
    # It emits the distinctive `<p>\n\begin{...}` signature (a newline right
    # after <p>), which the 'math' extension's environment output (`<p>\begin{`)
    # does not have, so this check is safe to run ungated.
    if (!has_math) has_math = isTRUE(any(grepl('<p>\n\\\\begin\\{', ret)))
    # math: detect the delimiters actually emitted into the output (inline
    # \(...\), display $$...$$, and \begin{} environments) instead of scanning
    # the source, which avoids loading a math library for a bare `$`, inline
    # code (`$x$`), or currency. This is gated on the 'math' extension being
    # enabled: when math is disabled (-latex_math), $$ / \begin{ in the output
    # is literal text, not math, and looks identical to real math. pkg_manual()
    # emits the same delimiters (\(...\), <p>$$...$$</p>) and runs with math
    # enabled by default, so it is covered too. Code blocks/spans are stripped
    # first because a math library ignores <code>/<pre> (a literal \( or $$
    # inside code is not rendered as math, so it must not trigger loading it).
    if (!has_math && 'math' %in% options$extensions) {
      r0 = gsub('(?s)<(pre|code)[ >].*?</\\1>', '', ret, perl = TRUE)
      has_math = length(r0) && (
        grepl('\\(', r0, fixed = TRUE) || grepl('$$', r0, fixed = TRUE) ||
        grepl('\\begin{', r0, fixed = TRUE)
      )
    }
    is_katex = TRUE
    if (has_math && length(js_math <- js_options(options[['js_math']], 'katex'))) {
      is_katex = js_math$package == 'katex'
    }
    # number figures and tables, etc.
    ret = number_refs(ret, r_ref, is_katex)
  } else if (format == 'latex') {
    if (isTRUE(options[['footnotes']])) ret = fix_footnotes(ret)  # fix footnotes
    r4 = sprintf(
      '(\\\\begin\\{verbatim}\n)%s(.+?)%s\n(.*?\n)(\\\\end\\{verbatim}\n)', id4, id4
    )
    # raw content blocks (```{=latex}/```{=tex}/```{=html}) are handled by the C
    # 'rawblock' extension, so only ordinary code blocks reach this verbatim
    # post-processing (which strips the id4-smuggled info string).
    ret = match_replace(ret, r4, function(x) {
      # TODO: support code highlighting for latex (listings or highr::hi_latex)
      gsub(r4, '\\1\\3\\4', x)
    }, perl = FALSE)
    # for nested verbatim code blocks, the inner blocks may have leftover ```\nid4
    ret = gsub(sprintf('(```)\n%s(.*?)%s', id4, id4), '\\1\\2', ret)
    # fix horizontal rules from --- (\linethickness doesn't work)
    ret = gsub('{\\linethickness}', '{1pt}', ret, fixed = TRUE)
    ret = redefine_level(ret, options[['top_level']])
    if (isTRUE(options[['toc']])) ret = paste0('\\tableofcontents\n', ret)
  }

  pkg_cite = yaml_field(yaml, format, 'citation_package')
  if (length(pkg_cite) != 1) pkg_cite = 'natbib'
  bib = yaml[['bibliography']]
  # temporarily save the bib values when previewing a book because bib may only
  # be specified in index.Rmd but not other chapters
  if (is.character(b <- .env$current_book)) {
    if (length(bib)) .env$bib[[b]] = bib else bib = .env$bib[[b]]
  }
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

  # use the template (if provided) to create a standalone document
  if (is.character(template)) {
    meta$body = I(ret)
    if (format == 'html') {
      # reset the internal js/css stored in acc_var() on exit
      on.exit(acc_var(), add = TRUE)
      # add js/css for math
      if (has_math) set_math(js_math, is_katex)
      # add js/css for syntax highlighting
      set_highlight(options, ret)
      # add js for mermaid
      if (has_mermaid && length(grep('mermaid', meta[['js']])) == 0)
        acc_var(js = '@npm/mermaid/dist/mermaid.min.js')
    }
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
    if (is_pdf) {
      tex = with_ext(output, '.tex')
      if (!isTRUE(yaml_field(yaml, format, 'keep_tex')))
        on.exit(file.remove(tex), add = TRUE)
      write_utf8(ret, tex)
      output = tinytex::latexmk(
        tex, latex_engine %||% 'xelatex',
        if (pkg_cite == 'biblatex') 'biber' else 'bibtex'
      )
    }
    # for RStudio to capture the output path when previewing the output
    if (is_rmd_preview()) message('\nOutput created: ', output)
    if (is_pdf) invisible(output) else write_utf8(ret, output)
  } else raw_string(ret, lang = paste0('.', format))
}

# insert body and meta variables into a template
build_output = function(format, options, template, meta, ...) {
  tpl = one_string(template, ...)
  if (format == 'html') {
    defaults = list(
      'css' = 'default',
      'lang' = locale_lang(),
      'plain-title' = I(str_trim(markdown_text(meta[['title']])))
    )
    for (i in setdiff(names(defaults), names(meta))) meta[[i]] = defaults[[i]]
    # special handling for css/js "files" that have no extensions
    for (i in c('css', 'js')) {
      i2 = paste0(i, '2')  # treat css2/js2 as global base (e.g. for sites)
      meta[[i]] = resolve_files(c(meta[[i2]], meta[[i]], acc_var(i)), i)
    }
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
#' See <https://pkg.yihui.org/litedown/book/#sec:markdown-options> for the full list of
#' options and their documentation.
#' @return A character vector of all available options.
#' @export
#' @examples
#' # all available options
#' litedown::markdown_options()
markdown_options = function() {
  # options enabled by default
  x1 = c(
    'smart', 'embed_resources', 'embed_cleanup', 'js_math', 'js_highlight', 'footnotes',
    'latex_math', 'auto_identifiers', 'cross_refs',
    # superscript/subscript/strikethrough are C extensions, so they appear in
    # list_extensions(); 'math' (via the 'latex_math' option) and 'rawblock'
    # (always on for html/latex) are not user-facing option names, so exclude
    # them from the auto-enabled extension list
    setdiff(list_extensions(), c('tagfilter', 'math', 'rawblock'))
  )
  # options disabled by default
  x2 = c(
    'toc', 'hardbreaks', 'tagfilter', 'number_sections', 'cleveref', 'offline',
    'smartypants'
  )
  sort(c(paste0('+', x1), paste0('-', x2)))
}

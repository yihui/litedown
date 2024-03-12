# reset an environment using "objects" in a list
reset_env = function(x = list(), envir) {
  rm(list = ls(envir, all.names = TRUE), envir = envir)
  list2env(x, envir)
}

# create a counter with methods
new_counter = function() {
  i = 0L
  list(
    get = function() i,
    inc = function() {
      i <<- i + 1L
    },
    reset = function() i <<- 0L
  )
}

# for unnamed code chunks
chunk_counter = new_counter()

# use PCRE by default (which seems to handle multibyte chars better)
gregexpr = function(..., perl = TRUE) base::gregexpr(..., perl = perl)

`%||%` = function(x, y) if (is.null(x)) y else x

dropNULL = function(x) x[!vapply(x, is.null, logical(1))]

eval_lang = function(x, envir) {
  if (is.symbol(x) || is.language(x)) eval(x, envir) else x
}

#' Convert some ASCII strings to HTML entities
#'
#' Transform ASCII strings `(c)` (copyright), `(r)` (registered trademark),
#' `(tm)` (trademark), and fractions `n/m` into *smart* typographic HTML
#' entities.
#' @param text A character vector of the Markdown text.
#' @return A character vector of the transformed text.
#' @examples
#' cat(litedown:::smartypants("1/2 (c)\n"))
smartypants = function(text) {
  text = xfun::split_lines(text)
  i = xfun::prose_index(text)
  r = '(?<!`)\\((c|r|tm)\\)|(\\d+/\\d+)(?!`)'
  text[i] = match_replace(text[i], r, function(z) {
    y = pants[z]
    i = is.na(y)
    y[i] = z[i]
    y
  })
  text
}

# Represent some fractions with HTML entities
fracs = local({
  n1 = c(
    '1/2', '1/3', '2/3', '1/4', '3/4', '1/5', '2/5', '3/5', '4/5', '1/6', '5/6',
    '1/8', '3/8', '5/8', '7/8'
  )
  n2 = c('1/7', '1/9', '1/10')
  x2 = seq_along(n2) + 8527  # &#8528;, 8529, 8530
  setNames(c(sprintf('&frac%s;', gsub('/', '', n1)), sprintf('&#%d;', x2)), c(n1, n2))
})

pants = c(fracs, c('(c)' = '&copy;', '(r)' = '&reg;', '(tm)' = '&trade;'))

# merge a later list in arguments into a former one by name
merge_list = function(...) {
  dots = list(...)
  res  = dots[[1]]
  for (i in seq_along(dots) - 1L) {
    if (i == 0) next
    x = dots[[i + 1]]
    if (!is.list(x)) next
    res[names(x)] = x
  }
  res
}

CHARS = c(letters, LETTERS, 0:9, '!', ',', '/', ':', ';', '=', '@')

# generate a random string that is not present in provided text
id_string = function(text, lens = c(5:10, 20), times = 20) {
  for (i in lens) {
    for (j in seq_len(times)) {
      id = paste(sample(CHARS, i, replace = TRUE), collapse = '')
      if (length(grep(id, text, fixed = TRUE)) == 0) return(id)
    }
  }
  # failure should be very rare
  stop('Failed to generate a unique ID string. You may try again.')
}

# a shorthand for gregexpr() and regmatches()
match_replace = function(x, pattern, replace = identity, ...) {
  m = gregexpr(pattern, x, ...)
  regmatches(x, m) = lapply(regmatches(x, m), function(z) {
    if (length(z)) replace(z) else z
  })
  x
}

# test if an input is a file path; if shouldn't be treated as file, use I()
is_file = function(x) {
  length(x) == 1 && !inherits(x, 'AsIs') && is.character(x) &&
    suppressWarnings(xfun::file_exists(x))
}

# make an output filename with the format name
auto_output = function(input, format) {
  ext = switch(format, commonmark = 'markdown', latex = 'tex', text = 'txt', format)
  output = xfun::with_ext(input, ext)
  check_output(input, output)
}

# make sure not to overwrite input file inadvertently
check_output = function(input, output) {
  if (xfun::same_path(input, output))
    stop('The output file path is the same as input: ', input)
  output
}

# substitute a variable in template `x` with its value; the variable may have
# more than one possible name, in which case we try them one by one
sub_var = function(x, name, value) {
  for (i in name) {
    if (any(grepl(i, x, fixed = TRUE))) {
      return(sub(i, one_string(value, test = TRUE), x, fixed = TRUE))
    }
  }
  x
}

# unescape HTML code
restore_html = function(x) {
  x = gsub('&quot;', '"', x, fixed = TRUE)
  x = gsub('&amp;', '&', x, fixed = TRUE)
  x = gsub('&lt;', '<', x, fixed = TRUE)
  x = gsub('&gt;', '>', x, fixed = TRUE)
  x
}

# find the first heading in html
first_heading = function(html) {
  m = regexpr(r <- '<(h[1-6])[^>]*?>(.+?)</\\1>', html, perl = TRUE)
  gsub(r, '\\2', regmatches(html, m))
}

.requireMathJS = function(x) {
  regs = c('\\\\\\(.+?\\\\\\)', '[$]{2}.+?[$]{2}', '\\\\\\[.+?\\\\\\]')
  for (i in regs) if (any(grepl(i, x, perl = TRUE))) return(TRUE)
  FALSE
}

# set js/css variables according to the js_math option
set_math = function(meta, options, html) {
  o = js_options(options[['js_math']], 'katex', .requireMathJS(html))
  if (is.null(o)) return(meta)
  if (is_katex <- o$package == 'katex')
    o$js = c(o$js, 'dist/contrib/auto-render.min.js')
  js = js_combine(
    sprintf('npm/%s%s/%s', o$package, o$version, o$js),
    if (is_katex) 'npm/@xiee/utils/js/render-katex.js'
  )
  css = sprintf('@npm/%s%s/%s', o$package, o$version, o$css)
  add_meta(meta, list(js = js, css = css))
}

# use jsdelivr's combine feature
js_combine = function(...) {
  if (length(x <- c(...))) paste0('@', paste(x, collapse = ','))
}

js_options = function(x, default, test) {
  d = js_default(x, default)
  x = if (is.list(x)) merge_list(d, x) else d
  if (is.null(x) || !test) return()
  if (x$version != '') x$version = sub('^@?', '@', x$version)
  x
}

js_default = function(x, default) {
  if (is.list(x)) x = x$package
  if (is.null(x) || isTRUE(x)) x = default
  if (is.character(x)) merge_list(js_libs[[x]], list(package = x))
}

js_libs = list(
  highlight = list(
    version = '11.7.0', style = 'xcode', js = 'build/highlight.min.js'
  ),
  katex = list(version = '', css = 'dist/katex.min.css', js = 'dist/katex.min.js'),
  mathjax = list(version = '3', js = 'es5/tex-mml-chtml.js'),
  prism = list(
    version = '1.29.0', js = 'components/prism-core.min.js'
  )
)

add_meta = function(x, v) {
  for (i in names(v)) x[[i]] = c(v[[i]], x[[i]])
  x
}

# set js/css variables according to the js_highlight option
set_highlight = function(meta, options, html) {
  r = '(?<=<code class="language-)([^"]+)(?=")'
  o = js_options(options[['js_highlight']], 'prism', any(grepl(r, html, perl = TRUE)))
  if (is.null(o)) return(meta)

  p = o$package
  # return jsdelivr subpaths
  get_path = function(path) {
    t = switch(
      p, highlight = 'gh/highlightjs/cdn-release%s/%s', prism = 'npm/prismjs%s/%s'
    )
    sprintf(t, o$version, path)
  }
  # add the `prism-` prefix if necessary
  normalize_prism = function(x) {
    if (length(x) == 1 && x == 'prism') x else sub('^(prism-)?', 'prism-', x)
  }

  # if resources need to be embedded, we need to work harder to figure out which
  # js files to embed (this is quite tricky and may not be robust)
  embed = 'https' %in% options[['embed_resources']]

  # style -> css
  css = if (is.null(s <- o$style)) {
    if (p == 'prism') 'prism-xcode'  # use prism-xcode.css in this package
  } else if (is.character(s)) js_combine(get_path(switch(
    p,
    highlight = sprintf('build/styles/%s.min.css', s),
    prism = sprintf('themes/%s.min.css', normalize_prism(s))
  )))

  # languages -> js
  get_lang = function(x) switch(
    p,
    highlight = sprintf('build/languages/%s.min.js', x),
    prism = sprintf('components/%s.min.js', normalize_prism(x))
  )
  autoloader = 'plugins/autoloader/prism-autoloader.min.js'
  o$js = c(o$js, if (!is.null(l <- o$languages)) get_lang(l) else {
    # detect <code> languages in html and load necessary language components
    lang = unique(unlist(regmatches(html, gregexpr(r, html))))
    f = switch(p, highlight = js_libs[[c(p, 'js')]], prism = autoloader)
    if (!embed && p == 'prism') f else {
      get_lang(lang_files(p, get_path(f), lang))
    }
  })
  js = get_path(o$js)
  if (p == 'highlight') js = c(js, 'npm/@xiee/utils/js/load-highlight.js')
  # do not combine js when they are automatically detected (this will make
  # embedding faster because each js is a separate URL that has been downloaded)
  js = if (is.null(l)) paste0('@', js) else js_combine(js)

  add_meta(meta, list(js = js, css = css))
}

# figure out which language support files are needed for highlight.js/prism.js
lang_files = function(package, path, langs) {
  u = jsdelivr(path, '')
  x = xfun::download_cache$get(u, 'text')
  x = one_string(x)

  warn = function(l1, l2, url) warning(
    "Unable to recognize code blocks with language(s): ", comma_list(l2),
    ". They will not be syntax highlighted by ", package, ".js. If you can find ",
    "the right language files at ", url, ", you may mangually specify their names ",
    "in the 'languages' field of the 'js_highlight' option.",
    if (length(l1)) c(" Also remember to add ", comma_list(l1))
  )

  if (package == 'highlight') {
    # first figure out all languages bundles in highlight.js (starting with grmr_)
    x = unlist(strsplit(x, ',\n?grmr_'))
    r = '^([[:alnum:]_-]+):.+'
    x = grep(r, x, value = TRUE)
    l = gsub(r, '\\1', x)
    # then find their aliases
    m = gregexpr('(?<=aliases:\\[)[^]]+(?=\\])', x)
    a = lapply(regmatches(x, m), function(z) {
      z = unlist(strsplit(z, '[",]'))
      z[!xfun::is_blank(z)]
    })
    l = c(l, unlist(a))  # all possible languages that can be highlighted
    l = setdiff(langs, l)  # languages not supported by default
    if (length(l) == 0) return()
    # check if language files exist on CDN
    d = paste0(dirname(u), '/languages/')
    l1 = unlist(lapply(l, function(z) {
      if (downloadable(sprintf('%s%s.min.js', d, z))) z
    }))
    l2 = setdiff(l, l1)
    if (length(l2)) warn(l1, l2, d)
    l1
  } else {
    # dependencies and aliases (the arrays should be more than 1000 characters)
    m = gregexpr('(?<=\\{)([[:alnum:]_-]+:\\[?"[^}]{1000,})(?=\\})', x)
    x = unlist(regmatches(x, m))
    if (length(x) < 2) {
      warning(
        "Unable to process Prism's autoloader plugin (", u, ") to figure out ",
        "language components automatically. Please report this message to ",
        packageDescription('litedown')$BugReports, "."
      )
      return()
    }
    x = x[1:2]
    m = gregexpr('([[:alnum:]_-]+):(\\["[^]]+\\]|"[^"]+")', x)
    x = lapply(regmatches(x, m), function(z) {
      z = gsub('[]["]', '', z)
      unlist(lapply(strsplit(z, '[:,]'), function(y) {
        setNames(list(y[-1]), y[1])
      }), recursive = FALSE)
    })
    # x1 is dependencies; x2 is aliases
    x1 = x[[1]]; x2 = unlist(x[[2]])
    # normalize aliases to canonical names
    i = langs %in% names(x2)
    langs[i] = x2[langs[i]]
    # resolve dependencies via recursion
    resolve_deps = function(lang) {
      deps = x1[[lang]]
      c(lapply(deps, resolve_deps), lang)
    }
    # all languages required for this page
    l1 = unique(unlist(lapply(langs, resolve_deps)))
    # languages that are officially supported
    l2 = c(names(x1), unlist(x1), x2)
    # for unknown languages, check if they exist on CDN
    d = sub('/plugins/.+', '/components/', u)
    l3 = unlist(lapply(setdiff(l1, l2), function(z) {
      if (!downloadable(sprintf('%sprism-%s.min.js', d, z))) z
    }))
    l4 = setdiff(l1, l3)
    if (length(l3)) warn(l4, l3, d)
    l4
  }
}

# test if a URL can be downloaded
downloadable = function(u, type = 'text') {
  !xfun::try_error(xfun::download_cache$get(u, type))
}

# quote a vector and combine by commas
comma_list = function(x) paste0('"', x, '"', collapse = ', ')

# get an option using a case-insensitive name
get_option = function(name, default = NULL) {
  x = options()
  i = match(tolower(name), tolower(names(x)))
  i = i[!is.na(i)]
  if (length(i) == 0) default else x[[i[1]]]
}

# if a string is a file path and test = TRUE, read the file; then concatenate elements by \n
one_string = function(x, by = '\n', test = FALSE) {
  if (test && is_file(x)) x = xfun::read_utf8(x)
  paste(x, collapse = by)
}

# find headings and build a table of contents as an unordered list
build_toc = function(html, n = 3) {
  if (n <= 0) return()
  if (n > 6) n = 6
  r = sprintf('<(h[1-%d])( id="[^"]+")?[^>]*>(.+?)</\\1>', n)
  items = unlist(regmatches(html, gregexpr(r, html)))
  if (length(items) == 0) return()
  x = gsub(r, '<toc\\2>\\3</toc>', items)  # use a tag <toc> to protect heading text
  h = as.integer(gsub('^h', '', gsub(r, '\\1', items)))  # heading level
  s = strrep('  ', seq_len(n) - 1)  # indent
  x = paste0(s[h], '- ', x)  # create an unordered list
  x = commonmark::markdown_html(x)
  # add anchors on TOC items
  x = gsub('<toc id="([^"]+)">(.+?)</toc>', '<a href="#\\1">\\2</a>', x)
  x = gsub('</?toc>', '', x)
  # add class 'numbered' to the first <ul> if any heading is numbered
  if (length(grep('<span class="section-number">', x)))
    x = sub('<ul>', '<ul class="numbered">', x)
  paste0('<div id="TOC">\n', x, '</div>')
}

# add TOC to the html body
add_toc = function(html, options) {
  o = options[['toc']]
  if (is.null(o) || isFALSE(o)) return(html)
  if (isTRUE(o)) o = list()
  if (!is.numeric(o$depth)) o$depth = 3
  one_string(c(build_toc(html, o$depth), html))
}

sec_levels = c('subsubsection', 'subsection', 'section', 'chapter', 'part')
# raise section levels: redefine section to chapter or part, and so on
redefine_level = function(x, top) {
  n = switch(top, chapter = 1, part = 2, 0)
  if (n == 0) return(x)
  for (i in 3:1) {
    x = gsub(sprintf('(^|\n)\\\\%s', sec_levels[i]), sprintf('\\\\%s', sec_levels[i + n]), x)
  }
  x
}

# move image attributes like `![](){#id .class width="20%"}`, heading attributes
# `# foo {#id .class}`, and fenced Div's `::: {#id .class}` into HTML tags and
# LaTeX commands
move_attrs = function(x, format = 'html') {
  if (format == 'html') {
    # images
    x = convert_attrs(x, '(<img src="[^>]+ )/>\\{([^}]+)\\}', '\\2', function(r, z, z2) {
      z1 = sub(r, '\\1', z)
      paste0(z1, z2, ' />')
    })
    # headings
    x = convert_attrs(x, '(<h[1-6])(>.+?) \\{([^}]+)\\}(</h[1-6]>)', '\\3', function(r, z, z3) {
      z1 = sub(r, '\\1 ', z)
      z24 = sub(r, '\\2\\4', z)
      paste0(z1, z3, z24)
    })
    # fenced Div's
    x = convert_attrs(x, '<p>:::+ \\{(.+?)\\}</p>', '\\1', function(r, z, z1) {
      # add attributes to the div but remove the data-latex attribute
      z1 = str_trim(gsub('(^| )data-latex="[^"]*"( |$)', ' ', z1))
      sprintf('<div %s>', z1)
    })
    x = gsub('<p>:::+</p>', '</div>', x)
  } else if (format == 'latex') {
    # only support image width
    x = convert_attrs(x, '(\\\\includegraphics)(\\{[^}]+\\})\\\\\\{([^}]+)\\\\\\}', '\\3', function(r, z, z3) {
      r2 = '(^|.* )width="([^"]+)"( .*|$)'
      j = grepl(r2, z3)
      w = gsub(r2, '\\2', z3[j])
      w = gsub('\\\\', '\\', w, fixed = TRUE)
      k = grep('%$', w)
      w[k] = paste0(as.numeric(sub('%$', '', w[k])) / 100, '\\linewidth')
      z3[j] = paste0('[width=', w, ']')
      z3[!j] = ''
      z1 = sub(r, '\\1', z)
      z2 = sub(r, '\\2', z)
      paste0(z1, z3, z2)
    }, format)
    # discard most attributes for headings
    r = sprintf('(\\\\(%s)\\{.+?) \\\\\\{([^}]+)\\\\\\}(\\})', paste(sec_levels, collapse = '|'))
    x = convert_attrs(x, r, '\\3', function(r, z, z3) {
      z = gsub(r, '\\1\\4', z)
      k = grepl('unnumbered', z3)
      z[k] = sub('{', '*{', z[k], fixed = TRUE)
      k = grepl('appendix', z3)
      z[k] = '\\appendix'
      z
    }, format)
    # fenced Div's
    r = '\n\\\\begin\\{verbatim\\}\n(:::+)( \\{([^\n]+?)\\})? \\1\n\\\\end\\{verbatim\\}\n'
    x = convert_attrs(x, r, '\\3', function(r, z, z3) {
      r3 = '(^|.*? )class="([^" ]+)[" ].*? data-latex="([^"]*)".*$'
      z3 = ifelse(
        grepl(r3, z3), gsub(r3, '{\\2}\\3', z3), ifelse(z3 == '', '', '{@}')
      )
      z3 = latex_envir(gsub('\\\\', '\\', z3, fixed = TRUE))
      z3[z3 %in% c('\\begin{@}', '\\end{@}')] = ''
      i = grep('^\\\\begin', z3)
      z3[i] = paste0('\n', z3[i])
      i = grep('^\\\\end', z3)
      z3[i] = paste0(z3[i], '\n')
      z3
    }, format)
  } else {
    # TODO: remove attributes for other formats
  }
  x
}

convert_attrs = function(x, r, s, f, format = 'html', f2 = identity) {
  r2 = '(?<=^| )[.#]([[:alnum:]-]+)(?= |$)'
  match_replace(x, r, function(y) {
    if (format == 'html') {
      z = gsub('[\U201c\U201d]', '"', y)
    } else {
      z = gsub('=``', '="', y, fixed = TRUE)
      z = gsub("''( |\\\\})", '"\\1', z)
      z = gsub('\\\\([#%])', '\\1', z)
    }
    z2 = f2(sub(r, s, z))
    # convert #id to id="" and .class to class=""
    z2 = match_replace(z2, r2, function(a) {
      i = grep('^[.]', a)
      if ((n <- length(i))) {
        # merge multiple classes into one class attribute
        a[i] = sub('^[.]', '', a[i])
        a[i] = c(rep('', n - 1), sprintf('class="%s"', paste(a[i], collapse = ' ')))
        a = c(a[i], a[-i])
      }
      if (length(i <- grep('^#', a))) {
        a[i] = gsub(r2, 'id="\\1"', a[i], perl = TRUE)
        a = c(a[i], a[-i])  # make sure id is the first attribute
      }
      a
    })
    f(r, z, str_trim(z2))
  })
}

str_trim = function(x) gsub('^\\s+|\\s+$', '', x)

# {A}, '', {B}, {C}, '', '' -> \begin{A}\end{A}\begin{B}\begin{C}\end{C}\end{B}
latex_envir = function(x, env = NULL) {
  n = length(x)
  if (n == 0) return()
  x1 = x[1]
  env2 = tail(env, 1)  # the most recent env is in the end
  env = if (x1 == '') head(env, -1) else c(env, sub('^(\\{[^}]+}).*$', '\\1', x1))
  c(if (x1 == '') paste0('\\end', env2) else paste0('\\begin', x1), latex_envir(x[-1], env))
}

# find and render footnotes for LaTeX output
render_footnotes = function(x) {
  f1 = f2 = NULL
  # [^1] is converted to {[}\^{}1{]}
  r = '(\n\n)(\\{\\[}\\\\\\^\\{}[0-9]+\\{\\]}): (.*?)\n(\n|$)'
  x = match_replace(x, r, function(z) {
    f1 <<- c(f1, sub(r, '\\2', z))
    f2 <<- c(f2, sub(r, '\\3', z))
    gsub(r, '\\1', z)
  }, perl = FALSE)
  for (i in seq_along(f1)) {
    x = sub(f1[i], sprintf('\\footnote{%s}', f2[i]), x, fixed = TRUE)
  }
  x
}

# add auto identifiers to headings
auto_identifier = function(x) {
  r = '<(h[1-6])([^>]*)>(.+?)</\\1>'
  match_replace(x, r, function(z) {
    z1 = sub(r, '\\1', z)  # tag
    z2 = sub(r, '\\2', z)  # attrs
    z3 = sub(r, '\\3', z)  # content
    i = !grepl(' id="[^"]*"', z2)  # skip headings that already have IDs
    id = unique_id(xfun::alnum_id(z3[i]), 'section')
    z[i] = sprintf('<%s id="%s"%s>%s</%s>', z1[i], id, z2[i], z3[i], z1[i])
    z
  })
}

# add a number suffix to an id if it is duplicated
unique_id = function(x, empty) {
  x[x == ''] = empty
  i = duplicated(x)
  for (d in unique(x[i])) {
    k = x == d
    x[k] = paste0(x[k], '_', seq_len(sum(k)))
  }
  x
}

# number sections in HTML output
number_sections = function(x) {
  m = gregexpr('</h[1-6]>', x)
  h = sub('</h([1-6])>', '\\1', unlist(regmatches(x, m)))
  if (length(h) == 0) return(x)  # no headings
  h = min(as.integer(h))  # highest level of headings
  r = '<h([1-6])([^>]*)>(?!<span class="section-number">)'
  n = rep(0, 6)  # counters for all levels of headings
  # test if a class name exists in attributes
  has_class = function(x, class) {
    grepl(sprintf(' class="([^"]+ )?%s( [^"]+)?"', class), x)
  }
  match_replace(x, r, function(z) {
    z1 = as.integer(sub(r, '\\1', z, perl = TRUE))
    z2 = sub(r, '\\2', z, perl = TRUE)
    num_sections = identity  # generate appendix numbers
    for (i in seq_along(z)) {
      k = z1[i]
      if (k < 6) n[(k + 1):6] <<- 0
      # skip unnumbered sections
      if (has_class(z2[i], 'unnumbered')) next
      if (has_class(z2[i], 'appendix')) {
        if (k != h) stop(
          "The 'appendix' attribute must be on the top-level heading (",
          strrep('#', h), ').'
        )
        num_sections = local({
          a = n[k]  # an offset
          # number headings with A-Z or roman numerals
          num = if (sum(z1[i:length(z)] == h) - 1 > length(LETTERS)) as.roman else {
            function(i) LETTERS[i]
          }
          function(s) {
            if (s[1] <= a) stop(
              'An appendix section must start with the top-level heading (',
              strrep('#', h), ').'
            )
            s[1] = num(s[1] - a)
            s
          }
        })
        next
      }
      n[k] <<- n[k] + 1
      # remove leading 0's
      s = if (h > 1) n[-(1:(h - 1))] else n
      s = paste(num_sections(s), collapse = '.')
      s = gsub('([.]0)+$', '', s)  # remove trailing 0's
      if (!grepl('[.]', s)) s = paste0(s, '.')  # '1. section' instead of '1 section'
      z[i] = paste0(z[i], sprintf('<span class="section-number">%s</span> ', s))
    }
    z
  })
}

#' @importFrom utils URLdecode
embed_resources = function(x, embed = 'local') {
  if (length(x) == 0) return(x)
  embed = c('https', 'local') %in% embed
  if (!any(embed)) return(x)

  r = '(<img[^>]* src="|<!--#[^>]*? style="background-image: url\\("?)([^"]+?)("|"?\\);)'
  x = match_replace(x, r, function(z) {
    z1 = sub(r, '\\1', z)
    z2 = sub(r, '\\2', z)
    z3 = sub(r, '\\3', z)
    # skip images already base64 encoded
    for (i in grep('^data:.+;base64,.+', z2, invert = TRUE)) {
      if (xfun::file_exists(f <- URLdecode(z2[i]))) {
        z2[i] = xfun::base64_uri(f)
      } else if (embed[1] && is_https(f)) {
        z2[i] = xfun::download_cache$get(f, 'base64')
      }
    }
    paste0(z1, z2, z3)
  })

  # CSS and JS
  r = paste0(
    '<link[^>]* rel="stylesheet" href="([^"]+)"[^>]*>|',
    '<script[^>]* src="([^"]+)"[^>]*>\\s*</script>'
  )
  x2 = NULL  # to be appended to x
  x = match_replace(x, r, function(z) {
    z1 = sub(r, '\\1', z)  # css
    z2 = sub(r, '\\2', z)  # js
    js = z2 != ''
    z3 = paste0(z1, z2)
    # skip resources already base64 encoded
    i1 = !grepl('^data:.+;base64,.+', z3)
    z3[i1] = gen_tags(z3[i1], ifelse(js[i1], 'js', 'css'), embed[1], embed[2])
    # for <script>s with defer/async, move them to the end of </body>
    i2 = grepl(' (defer|async)(>| )', z) & js
    x2 <<- c(x2, z3[i2])
    z3[i2] = ''
    z3
  })
  # move defer/async js to the end of <body>
  if (length(x2)) {
    x = if (length(grep('</body>', x)) != 1) {
      one_string(c(x, x2))
    } else {
      match_replace(x, '</body>', fixed = TRUE, perl = FALSE, function(z) {
        one_string(c(x2, z))
      })
    }
  }
  x
}

normalize_options = function(x, format = 'html') {
  g = get_option(sprintf('litedown.%s.options', format))
  x = option2list(x)
  n = names(x)
  n[n == 'hard_wrap'] = 'hardbreaks'
  n[n == 'tables'] = 'table'
  n[n == 'base64_images'] = 'embed_resources'
  names(x) = n
  # default options
  d = option2list(markdown_options())
  g = option2list(g)
  d[names(g)] = g  # merge global options() into default options
  d[n] = x  # then merge user-provided options
  if (!is.character(d[['top_level']])) d$top_level = 'section'
  # mathjax = true -> js_math = 'mathjax'
  if (isTRUE(d[['mathjax']])) d$js_math = 'mathjax'
  d$mathjax = NULL
  # highlight_code -> js_highlight
  if (!is.null(h <- d[['highlight_code']])) {
    h$package = 'highlight'
    d$js_highlight = h
    d$highlight_code = NULL
  }
  d = normalize_embed(d)
  # TODO: fully enable footnotes https://github.com/github/cmark-gfm/issues/314
  if (format == 'html' && !is.logical(d[['footnotes']])) d$footnotes = TRUE
  d
}

normalize_embed = function(x) {
  v = x[['embed_resources']]
  if (is.logical(v)) {
    v = if (v) 'local'
  } else {
    if (length(v) == 1 && v == 'all') v = c('local', 'https')
  }
  x[['embed_resources']] = v
  x
}

#' @import stats
namedBool = function(x, val = TRUE) as.list(setNames(rep(val, length(x)), x))

# normalize metadata variable names: change _ to -
normalize_meta = function(x) {
  # make sure some variables are available in metadata
  x = merge_list(list(classoption = '', documentclass = 'article'), x)
  names(x) = gsub('_', '-', names(x))
  x
}

# turn '+a-b c' to list(a = TRUE, b = FALSE, c = TRUE)
option2list = function(x) {
  if (!is.character(x)) return(as.list(x))
  x = unlist(strsplit(x, '\\b(?=[+-])', perl = TRUE))
  x = unlist(strsplit(x, '\\s+'))
  x = setdiff(x, '')
  i = grepl('^-', x)
  c(namedBool(sub('^[-]', '', x[i]), FALSE), namedBool(sub('^[+]', '', x[!i])))
}

pkg_file = function(...) {
  system.file(..., package = 'litedown', mustWork = TRUE)
}

jsdelivr = function(file, dir = 'gh/yihui/litedown/inst/resources/') {
  ifelse(is_https(file), file, sprintf('https://cdn.jsdelivr.net/%s%s', dir, file))
}

# if both @foo and foo are present, remove foo
resolve_dups = function(x) {
  x = unique(x)
  for (i in grep('^@', x, value = TRUE)) {
    x = x[x != sub('^@', '', i)]
  }
  x
}

# resolve CSS/JS shorthand filenames to actual paths (e.g., 'default' to 'default.css')
resolve_files = function(x, ext = 'css') {
  x = resolve_dups(x)
  if (length(x) == 0) return(x)
  # @foo -> jsdelivr.net/gh/yihui/litedown
  i0 = grepl('^@', x)
  x[i0] = sub('^@', '', x[i0])
  i = i0 & !grepl('/', x)
  x[i] = jsdelivr(xfun::with_ext(x[i], ext))
  # @foo/bar -> jsdelivr.net/foo/bar
  i = i0 & !grepl(',', x)
  x[i] = jsdelivr(x[i], '')
  # @foo/bar,baz -> jsdelivr.net/combine/foo/bar,foo/baz
  i = i0 & grepl(',', x)
  if (any(i)) x[i] = sapply(strsplit(x[i], ','), function(z) {
    d = dirname(z[1])
    for (j in 2:length(z)) {
      if (grepl('/', z[j])) {
        d = dirname(z[j])
      } else {
        z[j] = paste(d, z[j], sep = '/')
      }
    }
    paste0('combine/', paste(z, collapse = ','))
  })
  x[i] = jsdelivr(x[i], '')
  # built-in resources in this package
  i = dirname(x) == '.' & xfun::file_ext(x) == '' & !xfun::file_exists(x)
  x[i & (x == 'slides')] = 'snap'  # alias slides.css -> snap.css
  files = list.files(pkg_file('resources'), sprintf('[.]%s$', ext), full.names = TRUE)
  b = xfun::sans_ext(basename(files))
  if (any(!x[i] %in% b)) stop(
    "Invalid '", ext, "' option: ", paste0("'", setdiff(x[i], b), "'", collapse = ', '),
    " (possible values are: ", paste0("'", b, "'", collapse = ','), ")"
  )
  x[i] = files[match(x[i], b)]
  x = c(x[i], x[!i])
  x = if (ext %in% c('css', 'js')) gen_tags(x, ext) else xfun::read_all(x)
  I(x)
}

# generate tags for css/js depending on whether they need to be embedded or offline
gen_tag = function(x, ext = '', embed_https = FALSE, embed_local = FALSE) {
  if (ext == 'css') {
    t1 = '<link rel="stylesheet" href="%s">'
    t2 = c('<style type="text/css">', '</style>')
  } else if (ext == 'js') {
    t1 = '<script src="%s" defer></script>'
    t2 = c('<script>', '</script>')
  } else stop("The file extension '", ext, "' is not supported.")
  is_web = is_https(x)
  is_rel = !is_web && xfun::is_rel_path(x)
  if (is_web && embed_https && xfun::url_filename(x) == 'MathJax.js') {
    warning('MathJax.js cannot be embedded. Please use MathJax v3 instead.')
    embed_https = FALSE
  }
  if ((is_rel && !embed_local) || (is_web && !embed_https)) {
    # linking for 1) local rel paths that don't need to be embedded, or 2) web
    # resources that don't need to be accessed offline
    sprintf(t1, x)
  } else {
    # embedding for other cases
    one_string(c(t2[1], resolve_external(x, is_web, ext), t2[2]))
  }
}

is_https = function(x) grepl('^https://', x)

# a vectorized version
gen_tags = function(...) mapply(gen_tag, ...)

# read CSS/JS and embed external fonts/background images, etc.
resolve_external = function(x, web = TRUE, ext = '') {
  # download and cache web resources
  txt = if (web) xfun::download_cache$get(x, 'text', handler = function(code) {
    # remove jsdelivr comments
    if (grepl('^https://cdn[.]jsdelivr[.]net/', x)) {
      code = gsub(
        '^/[*][*]\n( [*][^\n]*\n)+ [*]/\n|\n/[*/]# sourceMappingURL=.+[.]map( [*]/)?$',
        '', one_string(code)
      )
      code = base64_url(x, code, ext)
    }
    code
  }) else {
    base64_url(x, xfun::read_utf8(x), ext)
  }
}

# find url("path") in JS/CSS and base64 encode the resources
base64_url = function(url, code, ext) {
  d = dirname(url)
  # embed fonts in mathjax's js
  if (grepl('^https://cdn[.]jsdelivr[.]net/npm/mathjax.+[.]js$', url)) {
    r = '.*?fontURL:[^"]+\\("([^"]+)".*'  # output/chtml/fonts/woff-v2
    p = xfun::grep_sub(r, '\\1', code)
    if (length(p) == 1) code = match_replace(
      code, '(?<=src:\'url\\(")(%%URL%%/[^"]+)(?="\\))', function(u) {
        u = sub('%%URL%%', paste(d, p, sep = '/'), u, fixed = TRUE)
        unlist(lapply(u, function(x) xfun::download_cache$get(x, 'base64')))
      }
    ) else warning(
      'Unable to determine the font path in MathJax. Please report an issue to ',
      'https://github.com/yihui/litedown/issues and mention the URL ', url, '.'
    )
  }
  # find `attr: url(resource)` and embed url resources in CSS
  if (ext == 'css') {
    r = '(: ?url\\("?)([^)]+)("?\\))'
    code = match_replace(code, r, function(z) {
      z1 = gsub(r, '\\1', z)
      z2 = gsub(r, '\\2', z)
      z3 = gsub(r, '\\3', z)
      i = !is_https(z2)
      z2[i] = paste(d, z2[i], sep = '/')
      z2 = unlist(lapply(z2, function(x) {
        if (is_https(x)) xfun::download_cache$get(x, 'base64') else xfun::base64_uri(x)
      }))
      paste0(z1, z2, z3)
    })
  }
  code
}

# resolve HTML dependencies and write out the appropriate HTML code to `header-includes`
add_html_deps = function(meta, output, embed = TRUE) {
  if (!xfun::loadable('knitr')) return(meta)
  deps = c(knitr::knit_meta(), .env$knit_meta)
  if (length(deps) == 0 || !any(vapply(deps, inherits, logical(1), 'html_dependency'))) return(meta)
  if (!xfun::loadable('rmarkdown')) stop(
    'It seems the document contains HTML dependencies, which require ',
    'the rmarkdown package but it is not available.'
  )
  deps = rmarkdown::flatten_html_dependencies(deps)
  deps = rmarkdown::html_dependency_resolver(deps)
  if (length(deps) == 0) return(meta)
  d1 = d2 = NULL
  # if resources need to be embedded, use their absolute paths; otherwise copy
  # dependencies to 'libs/' and use relative paths
  if (!embed) {
    if (is.character(output)) {
      owd = setwd(dirname(output)); on.exit(setwd(owd), add = TRUE)
    }
    d1 = 'libs'; d2 = '.'
  }
  deps = rmarkdown::html_dependencies_as_string(deps, d1, d2)
  meta[['header-includes']] = paste(deps, one_string(meta[['header-includes']], test = TRUE), sep = '\n')
  meta
}

# compact HTML code
clean_html = function(x) {
  x = gsub('\n+(\n<[a-z1-6]+[^>]*>|\n</(body|div|head|html)>)', '\\1', x)
  # can also merge <style>/<script> tags (<style type="text/css">).+?</style>\\s*\\1
  x
}

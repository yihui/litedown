new_env = function(...) new.env(..., parent = emptyenv())

#' Parse R Markdown or R scripts
#'
#' Parse input into code chunks, inline code expressions, and text fragments:
#' [crack()] is for parsing R Markdown, and [sieve()] is for R scripts.
#'
#' For R Markdown, a code chunk must start with a fence of the form ````
#' ```{lang} ````, where `lang` is the language name, e.g., `r` or `python`. The
#' body of a code chunk can start with chunk options written in "pipe comments",
#' e.g., `#| eval = TRUE, echo = FALSE` (the CSV syntax) or `#| eval: true` (the
#' YAML syntax). An inline code fragment is of the form `` `{lang} source` ``
#' embedded in Markdown text.
#' @inheritParams mark
#' @export
#' @return A list of code chunks and text blocks:
#'
#'   - Code chunks are of the form `list(source, type = "code_chunk", options,
#'   comments, ...)`: `source` is a character vector of the source code of a
#'   code chunk, `options` is a list of chunk options, and `comments` is a
#'   vector of pipe comments.
#'
#'   - Text blocks are of the form `list(source, type = "text_block", ...)`. If
#'   the text block does not contain any inline code, `source` will be a
#'   character string (lines of text concatenated by line breaks), otherwise it
#'   will be a list with members that are either character strings (normal text
#'   fragments) or lists of the form `list(source, options, ...)` (`source` is
#'   the inline code, and `options` contains its options specified inside ``
#'   `{lang, ...}` ``).
#'
#'   Both code chunks and text blocks have a list member named `lines` that
#'   stores their starting and ending line numbers in the input.
#' @examples
#' library(litedown)
#' # parse R Markdown
#' res = crack(c('```{r}\n1+1\n```', 'Hello, `pi` = `{r} pi` and `e` = `{r} exp(1)`!'))
#' str(res)
#' # evaluate inline code and combine results with text fragments
#' txt = lapply(res[[2]]$source, function(x) {
#'   if (is.character(x)) x else eval(parse(text = x$source))
#' })
#' paste(unlist(txt), collapse = '')
crack = function(input, text = NULL) {
  text = read_input(input, text); input = attr(text, 'input')
  xml = commonmark::markdown_xml(text, sourcepos = TRUE)
  rx_engine = '([a-zA-Z0-9_]+)'  # only allow these characters for engine names
  r = paste0(
    '<(code|code_block) sourcepos="(\\d+):(\\d+)-(\\d+):(\\d+)"( info="[{]+',
    rx_engine, '[^"]*?[}]")? xml:space="[^>]*>([^<]*)<'
  )
  m = match_all(xml, r, perl = TRUE)[[1]] %|% matrix(character(), 9)
  # code blocks must have non-empty info strings
  m = m[, m[2, ] != 'code_block' | m[8, ] != '', drop = FALSE]

  res = list()
  # add a block of text and the line range info
  add_block = function(l1, l2, ...) {
    res[[length(res) + 1]] <<- list(source = text[l1:l2], ..., lines = c(l1, l2))
    res
  }

  n = length(text)
  i = 1L  # the possible start line number of text blocks
  for (j in which(m[2, ] == 'code_block')) {
    # start (3) and end (5) line numbers for code chunks
    pos = as.integer(m[c(3, 5), j]); i1 = pos[1]; i2 = pos[2]
    # add the possible text block before the current code chunk
    if (i1 > i) add_block(i, i1 - 1L, type = 'text_block')
    # add the code chunk
    add_block(i1, i2, info = m[8, j], type = 'code_chunk')
    i = i2 + 1L  # the earliest line for the next text block is next line
  }
  # if there are lines remaining, they must be a text block
  if (i <= n) add_block(i, n, type = 'text_block')

  if (!length(m)) return(res)

  set_error_handler(input)

  m = m[, m[2, ] == 'code', drop = FALSE]
  # find out inline code `{lang} expr`
  rx_inline = '^\\s*[{](.+?)[}]\\s+(.+?)\\s*$'
  # look for `r expr` if `{lang}` not found (for compatibility with knitr)
  if (!any(j <- grepl(rx_inline, m[9, ])) && getOption('litedown.enable.knitr_inline', FALSE)) {
    rx_inline = '^(r) +(.+?)\\s*$'
    j = grepl(rx_inline, m[9, ])
  }
  m = m[, j, drop = FALSE]
  n_start = uapply(res, function(x) x$lines[1])  # starting line numbers
  j = findInterval(m[3, ], n_start)  # find which block each inline code belongs to
  for (i in seq_len(ncol(m))) {
    b = res[[j[i]]]; l = b$lines
    # column position is based on bytes instead of chars; needs to be adjusted to the latter
    pos = char_pos(text, as.integer(m[3:6, i]))
    i1 = pos[1]; i2 = pos[3]
    # commonmark::markdown_xml(sourcepos = TRUE) gives wrong column info when
    # the line has leading spaces (which are ignored), so doublecheck here (in
    # theory we also need to consider the case i1 != i2 but that's a little too
    # complicated and may be rare, too)
    if (i1 == i2 && grepl('^\\s+', ti <- text[i1])) {
      mi = restore_html(m[9, i])
      if (substring(ti, pos[2], pos[4]) != mi) {
        p = base::gregexpr(mi, ti, fixed = TRUE)[[1]]
        if (length(p) > 1 || p < 1) {
          save_pos(c(i1, i2)); stop(
            'Unable to locate the inline code expression ', mi,
            '. Please file an issue to https://github.com/yihui/litedown/issues ',
            'with a minimal reproducible example.'
          )
        }
        pos[2] = p; pos[4] = p + attr(p, 'match.length') - 1
      }
    }
    s = nchar(b$source)
    # calculate new position of code after we concatenate all lines of this block by \n
    b$col = c(b$col, c(
      sum(s[seq_len(i1 - l[1])] + 1) + pos[2],
      sum(s[seq_len(i2 - l[1])] + 1) + pos[4]
    ))
    b$pos = c(b$pos, pos)
    res[[j[i]]] = b
  }

  i1 = 0  # code chunk index
  # remove code fences, and extract code in text blocks
  for (j in seq_along(res)) {
    b = res[[j]]
    if (b$type == 'code_chunk') {
      code = b$source
      N = length(code)
      # a code block may be indented or inside a blockquote
      p = grep_sub('^([\t >]*)(`{3,}|~{3,}).*', '\\1\\2', code[1])
      if (length(p) == 0) stop('Possibly malformed code block fence: ', code[1])
      if (!grepl(sub('^[\t >]*', '', p), code[N])) stop(
        'The fences of the code block do not match:\n\n', code[1], '\n', code[N]
      )
      p = gsub('[`~]+$', '', p)
      if (p != '') {
        i = startsWith(code, p)
        # remove indentation or >
        code[i] = substr(code[i], nchar(p) + 1, nchar(code[i]))
        # trailing spaces in the prefix may have been trimmed: yihui/knitr#1446
        code[!i] = gsub(gsub('(.+?)\\s+$', '^\\1', p), '', code[!i])
        b$prefix = p
      }
      # possible comma-separated chunk options in header
      rx_opts = paste0('^(`{3,}|~{3,})\\s*([{]+)', rx_engine, '(.*?)\\s*[}]+\\s*$')
      o = match_one(code[1], rx_opts)[[1]]
      if (length(o)) {
        # if two or more `{` is used, we will write chunk fences to output
        if (nchar(o[3]) > 1) b$fences = c(
          sub('{{', '{', sub('}}\\s*$', '}', code[1]), fixed = TRUE), code[N]
        )
        o = if (o[5] != '') csv_options(o[5])
      }
      code = code[-c(1, N)]  # remove fences
      save_pos(b$lines)
      code = split_chunk(b$info, code)
      b[c('source', 'options', 'comments')] = code[c('code', 'options', 'src')]
      # starting line number of code
      b$code_start = b$lines[1] + 1L + length(b$comments)
      # default label is chunk-i (or parent-label-i for child documents)
      i1 = i1 + 1
      # merge chunk options from header with pipe comment options
      b$options = merge_list(list(
        label = sprintf('%s-%d', (if (isTRUE(.env$child)) reactor('label')) %||% 'chunk', i1)
      ), o, b$options)
      b$options$engine = b$info
      b$info = NULL  # the info is stored in chunk options as `engine`
    } else if (length(p <- b$col) > 0) {
      p = matrix(p, nrow = 2)
      x = one_string(b$source)
      x1 = substring(x, p[1, ], p[2, ])  # code
      # then extract normal text
      p = rbind(p[1, ] - 1, p[2, ] + 1)
      p = matrix(c(1, p, nchar(x)), nrow = 2)
      x2 = substring(x, p[1, ], p[2, ])  # text
      # get rid of left-over backticks
      N = length(x2)
      x2[1] = gsub('`+$', '', x2[1])  # trailing ` of first
      x2[N] = gsub('^`+', '', x2[N])  # leading ` of last
      # ` at both ends for text in the middle
      if (N > 2) x2[2:(N - 1)] = gsub('^`+|`+$', '', x2[2:(N - 1)])
      # see if the code is wrapped in $ $
      d1 = substring(x2, nchar(x2), nchar(x2)) == '$'
      d2 = substring(x2, 1, 1) == '$'
      dollar = d1[-N] & d2[-1]
      # position of code c(row1, col1, row2, col2)
      pos = matrix(b$pos, nrow = 4)
      x = as.list(head(c(rbind(x2, c(x1, ''))), -1))
      for (i in seq_len(N - 1)) {
        z = match_one(x1[i], rx_inline)[[1]][-1]
        p2 = pos[, i]; save_pos(p2)
        xi = list(
          source = z[2], pos = p2,
          options = csv_options(gsub('^([^,]+)', 'engine="\\1"', z[1]))
        )
        if (dollar[i]) xi$math = TRUE
        x[[2 * i]] = xi
      }
      b$source = x
    } else {
      b$source = paste(b$source, collapse = '\n')
    }
    b$pos = b$col = NULL  # positions not useful anymore
    res[[j]] = b
  }
  res
}

set_error_handler = function(input) {
  opts = options(xfun.handle_error.loc_fun = get_loc)
  oenv = as.list(.env)
  exit_call(function() { options(opts); reset_env(oenv, .env) })
  .env$input = input  # store the input name for get_loc()
}

# convert byte position to character position
char_pos = function(x, p) {
  x2 = x[p[c(1, 3)]]
  # no need to convert if no multibyte chars
  if (all(nchar(x2) == nchar(x2, 'bytes'))) return(p)
  p2 = p[c(2, 4)]
  Encoding(x2) = 'bytes'
  x2 = substr(x2, 1, p2 - 1)  # go back one char in case current column is multibyte
  Encoding(x2) = 'UTF-8'
  p[c(2, 4)] = nchar(x2) + 1L  # go forward by one char
  if (p2[2] == 0) p[4] = 0L  # boundary case: \n before the closing backtick
  p
}

#' @details For R scripts, text blocks are extracted by removing the leading
#'   `#'` tokens. All other lines are treated as R code, which can optionally be
#'   separated into chunks by consecutive lines of `#|` comments (chunk options
#'   are written in these comments). If no `#'` or `#|` tokens are found in the
#'   script, the script will be divided into chunks that contain smallest
#'   possible complete R expressions.
#' @note For simplicity, [sieve()] does not support inline code expressions.
#'   Text after `#'` is treated as pure Markdown.
#'
#'   It is a pure coincidence that the function names `crack()` and `sieve()`
#'   weakly resemble Carson Sievert's name, but I will consider adding a class
#'   name `sievert` to the returned value of `sieve()` if Carson becomes the
#'   president of the United States someday, which may make the value
#'   radioactive and introduce a new programming paradigm named _Radioactive
#'   Programming_ (in case _Reactive Programming_ is no longer fun or cool).
#' @rdname crack
#' @export
#' @examples
#'
#' # parse R code
#' res = sieve(c("#' This is _doc_.", '', '#| eval=TRUE', '# this is code', '1 + 1'))
#' str(res)
sieve = function(input, text = NULL) {
  text = read_input(input, text); input = attr(text, 'input')
  n = length(text)
  r = run_range(grepl("^#'( .+| *)$", text), is_blank(text))
  nc = ncol(r)

  # no #' or #|: split code into smallest expressions
  if (nc == 0 && !any(startsWith(text, '#| '))) {
    res = xfun::split_source(text, TRUE, TRUE)
    res = .mapply(function(code, label) {
      l = attr(code, 'lines')
      list(
        source = c(code), type = 'code_chunk', lines = l, code_start = l[1],
        options = list(engine = 'r', label = label)
      )
    }, res, sprintf('chunk-%d', seq_along(res)))
    return(res)
  }

  # split doc and code by #', and divide code by #|
  res = list()
  add_block = function(l1, l2, type = 'code_chunk', pipe = FALSE) {
    x = text[l1:l2]
    if (type == 'text_block') {
      el = list(source = one_string(sub("^#' ?", '', x)))
    } else {
      if (all(i <- is_blank(x))) return()
      # trim blank lines at both ends
      i2 = range(which(!i))  # first and last non-empty lines
      l1 = l1 + (i2[1] - 1)
      l2 = l2 - (length(i) - i2[2])
      save_pos(c(l1, l2))
      x = text[l1:l2]
      el = if (pipe) partition(x) else list(source = x)
      el$code_start = as.integer(l1) + length(el$comments)
      el$options$engine = 'r'
    }
    el$type = type
    el$lines = as.integer(c(l1, l2))
    res[[length(res) + 1]] <<- el
  }
  partition = function(code) {
    code = split_chunk('r', code)
    set_names(code[c('code', 'options', 'src')], c('source', 'options', 'comments'))
  }
  # detect #| and split a block of code into chunks
  add_chunk = function(l1, l2) {
    x = text[l1:l2]; N = length(x)
    k = run_range(startsWith(x, '#| '))[1, ]
    if ((n <- length(k)) == 0) return(add_block(l1, l2))
    if (k[1] > 1) { k = c(1, k); n = n + 1 }  # make sure to scan from start
    for (i in seq_len(n)) {
      add_block(l1 - 1 + k[i], if (i == n) l2 else l1 - 1 + k[i + 1] - 1, pipe = TRUE)
    }
  }

  set_error_handler(input)

  i = 1
  for (j in seq_len(nc)) {
    i1 = r[1, j]; i2 = r[2, j]
    if (i1 > i) add_chunk(i, i1 - 1)
    add_block(i1, i2, type = 'text_block')
    i = i2 + 1
  }
  if (i <= n) add_chunk(i, n)
  # add possibly missing chunk labels
  i = vapply(res, function(x) x$type == 'code_chunk', FALSE)
  res[i] = .mapply(function(x, label) {
    if (is.null(x$options$label)) x$options$label = label
    x
  }, res[i], sprintf('chunk-%d', seq_len(sum(i))))
  res
}

# ranges of TRUE runs in a logical vector
run_range = function(x, extend = NULL) {
  r = rle(x); l = r$lengths; i = r$values
  i2 = cumsum(l); i1 = i2 - l + 1
  i1 = i1[i]; i2 = i2[i]
  # include adjacent blank lines, e.g., a blank line before or after #' should be doc
  if (!is.null(extend)) {
    k = extend[pmax(i1 - 1, 1)]  # check if previous line is empty
    i1[k] = i1[k] - 1
    k = extend[pmin(i2 + 1, length(extend))]  # check if next line is empty
    i2[k] = i2[k] + 1
    # merge adjacent runs, e.g., 1:2 and 2:4 -> 1:4
    k = NULL
    if ((n <- length(i1)) > 1) {
      k = i2[1:(n - 1)] < i1[2:n]
      i1 = i1[c(TRUE, k)]; i2 = i2[c(k, TRUE)]
    }
  }
  rbind(i1, i2)
}

# convert knitr's inline `r code` to litedown's `{r} code`
convert_knitr = function(input) {
  x = read_utf8(input)
  r = '(?<!(^``))(?<!(\n``))`r[ #]([^`]+)\\s*`'
  i = prose_index(x)
  x[i] = gsub(r, '`{r} \\3`', x[i], perl = TRUE)
  write_utf8(x, input)
}

#' Get the `fuse()` context
#'
#' A helper function to query the [fuse()] context (such as the input file path
#' or the output format name) when called inside a code chunk.
#' @param item The name of the context item.
#' @return If the `item` is provided, return its value in the context. If
#'   `NULL`, the whole context (an environment) is returned.
#' @export
#' @examples
#' litedown::get_context('input')
#' litedown::get_context('format')
#' names(litedown::get_context())  # all available items
get_context = function(item = NULL) if (is.null(item)) .env else .env[[item]]

# return a string to indicate the error location
get_loc = function(label) {
  l = .env$source_pos; n = length(l)
  if (n == 4) l = sprintf('#%d:%d-%d:%d', l[1], l[2], l[3], l[4])  # row1:col1-row2:col2
  if (n == 2) l = sprintf('#%d-%d', l[1], l[2])  # row1-row2
  paste0(.env$input2 %|% .env$input, l, if (label != '') paste0(' [', label, ']'))
}

# save line numbers in .env to be used in error messages
save_pos = function(x) .env$source_pos = x

# line/col info for ANSI links
link_pos = function() {
  l = .env$source_pos
  sprintf('line = %d:col = %d', l[1], if (length(l) == 4) l[2] else 1)
}

# get the execution order of code/text blocks via the `order` option (lower
# values indicate earlier execution)
block_order = function(res, N = length(res)) {
  check = function(b, env) {
    if (is.null(o <- b$options[['order']])) return(NA)
    if (is_lang(o)) o = eval(o, env)
    if (length(o) == 1 && is.numeric(o)) return(o)
    save_pos(b$pos %||% b$lines)
    stop("The chunk option 'order' must be either NULL or a number.", call. = FALSE)
  }
  x = seq_len(N); e = new.env(parent = fuse_env()); e$N = N
  for (i in x) {
    b = res[[i]]; e$i = i
    if (b$type == 'code_chunk') {
      if (!is.na(o <- check(b, e))) x[i] = o
    } else if (is.list(b$source)) {
      # use the first found `order` option in all inline expressions
      for (s in b$source) if (is.list(s) && !is.na(o <- check(s, e))) {
        x[i] = o; break
      }
    }
  }
  order(x)
}

#' @description The function `fuse()` extracts and runs code from code chunks
#'   and inline code expressions in R Markdown, and interweaves the results with
#'   the rest of text in the input, which is similar to what `knitr::knit()` and
#'   `rmarkdown::render()` do. It also works on R scripts in a way similar to
#'   `knitr::spin()`. The function `fiss()` extracts code from the input, and is
#'   similar to `knitr::purl()`.
#' @rdname mark
#' @param envir An environment in which the code is to be evaluated. It can be
#'   accessed via [fuse_env()] inside [fuse()].
#' @param quiet If `TRUE`, do not show the progress bar. If `FALSE`, the
#'   progress bar will be shown after a number of seconds, which can be set via
#'   a global [option][options] `litedown.progress.delay` (the default is `2`).
#'   THe progress bar output can be set via a global option
#'   `litedown.progress.output` (the default is [stderr()]).
#' @seealso [sieve()], for the syntax of R scripts to be passed to [fuse()].
#' @export
#' @examples
#' library(litedown)
#' doc = c('```{r}', '1 + 1', '```', '', '$\\pi$ = `{r} pi`.')
#' fuse(doc)
#' fuse(doc, '.tex')
#' fiss(doc)
fuse = function(input, output = NULL, text = NULL, envir = parent.frame(), quiet = FALSE) {
  text = read_input(input, text); input = attr(text, 'input')
  # determine if the input is R or R Markdown
  if (r_input <- is_R(input, text)) {
    blocks = sieve(input, text)
    yaml = yaml_body(blocks[[1]]$source)$yaml
  } else {
    blocks = crack(input, text)
    yaml = yaml_body(text)$yaml
  }
  full = is_output_full(output)
  format = detect_format(output, yaml)
  output = auto_output(input, output, format)
  if (!is.null(output_base <- output_path(input, output)))
    output_base = sans_ext(output_base)

  opts = reactor()
  # clean up the figure folder on exit if it's empty
  on.exit(del_empty_dir({
    if (dir.exists(fig.dir <- opts$fig.path)) fig.dir else dirname(fig.dir)
  }), add = TRUE)

  # restore and clean up some objects on exit
  opts2 = as.list(opts); on.exit(reactor(opts2), add = TRUE)
  oenv = as.list(.env); on.exit(reset_env(oenv, .env), add = TRUE)

  # set working directory if unset
  if (is_file(input) && is.null(opts$wd)) opts$wd = dirname(normalizePath(input))

  # store output dir so we can calculate relative paths for plot files later
  .env$wd_out = normalize_path(
    if (is.null(output_base)) {
      if (is.character(.env$wd_out)) .env$wd_out else '.'
    } else dirname(output_base)
  )
  # store the environment and output format
  .env$global = envir; .env$format = format

  # set default device to 'cairo_pdf' for LaTeX output, and 'png' for other formats
  if (is.null(opts$dev)) {
    opts$dev = if (format == 'latex') 'cairo_pdf' else 'png'
  }
  # set default figure and cache paths
  set_path = function(name) {
    # fig.path = output__files/ if `output` is a path, otherwise use
    # litedown__files/ (we don't use _files because of rstudio/rmarkdown#2550)
    if (is.null(p <- opts[[name]])) p = aux_path(output_base %||% 'litedown', name)
    slash = endsWith(p, '/')
    # make sure path is absolute so it will be immune to setwd() (in code chunks)
    if (is_rel_path(p)) {
      p = file.path(getwd(), p)
      # preserve trailing slash because file.path() removes it on Windows
      if (slash) p = sub('/*$', '/', p)
    }
    opts[[name]] = p
  }
  set_path('fig.path'); set_path('cache.path')

  .env$input = input
  res = .fuse(blocks, input, quiet)

  # save timing data if necessary
  timing_data()

  # keep the markdown output if keep_md = TRUE is set in YAML output format
  if (is_output_file(output) && isTRUE(yaml_field(yaml, format, 'keep_md'))) {
    write_utf8(res, with_ext(output, '.md'))
  }
  fuse_output(input, output, res, full)
}

# default fig/cache path
aux_path = function(base = sans_ext(file), type, file) {
  paste0(base, c(fig.path = '__files/', cache.path = '__cache/')[type])
}
fig_path = function(path) aux_path(, 'fig.path', path)

# if output = '.md' or 'markdown', no need for further mark() conversion
fuse_output = function(input, output, res, full = NULL) {
  if (is.character(output) && grepl('[.]md$|^markdown$', output)) {
    if (is_output_file(output)) {
      write_utf8(res, output)
    } else raw_string(res)
  } else {
    if (isTRUE(full)) attr(output, 'full') = TRUE
    mark(input, output, res)
  }
}

#' @rdname mark
#' @export
fiss = function(input, output = '.R', text = NULL) {
  text = read_input(input, text); input = attr(text, 'input')
  output = auto_output(input, output, NULL)
  blocks = crack(input, text)
  # TODO: what should we do for non-R code? also consider eval=FALSE and error=TRUE
  res = uapply(blocks, function(b) {
    if (b$type == 'code_chunk' && !isFALSE(b$options$purl) && b$options$engine == 'r')
      c(b$source, '')
  })
  if (is_output_file(output)) write_utf8(res, output) else raw_string(res)
}

.fuse = function(blocks, input, quiet) {
  n = length(blocks)
  nms = vapply(blocks, function(x) x$options[['label']] %||% '', character(1))
  names(blocks) = nms

  # a simple progress indicator: we need to know how many spaces we need to wipe
  # out previous progress text of the form:
  # xxx% | input_file#line1-line2 [label]
  # ...4..3          1     1     .2     1
  p_len = 4 + 3 + sum(nchar(input)) + 1 +
    (sum(nchar(sprintf('%d', blocks[[n]]$lines))) + 1) + 2 + max(nchar(nms)) + 1
  p_clr = paste0('\r', strrep(' ', p_len), '\r')  # a string to clear the progress
  p_out = getOption('litedown.progress.output', stderr())
  p_yes = FALSE; t0 = Sys.time(); td = getOption('litedown.progress.delay', 2)
  p_bar = function(x) {
    if (!quiet && (p_yes || Sys.time() - t0 > td)) {
      cat(x, sep = '', file = p_out)
      p_yes <<- TRUE
    }
  }
  # if error occurs, print error location with a clickable file link
  k = 0  # when exiting, k should be n + 1
  on_error = function() {
    if (k > n) return()  # blocks have been successfully fused
    p_bar(p_clr)
    ansi_link(input)
    message('Quitting from ', get_loc(nms[k]))
  }
  # suppress tidyverse progress bars and use cairo for bitmap devices (for
  # smaller plot files and possible parallel execution)
  opt = options(rstudio.notebook.executing = TRUE, bitmapType = bitmap_type())
  on.exit({ options(opt); on_error() }, add = TRUE)

  # the chunk option `order` determines the execution order of chunks
  o = block_order(blocks, n)
  res = character(n)
  for (i in seq_len(n)) {
    k = o[i]; b = blocks[[k]]; save_pos(b$lines)
    p_bar(c(as.character(round((i - 1)/n * 100)), '%', ' | ', get_loc(nms[k])))
    # record timing if requested
    if (!isFALSE(time <- timing_path())) t1 = Sys.time()
    res[k] = if (b$type == 'code_chunk') {
      one_string(fuse_code(b, blocks))
    } else {
      one_string(fuse_text(b), '')
    }
    if (!isFALSE(time)) record_time(Sys.time() - t1, b$lines, nms[k])
    p_bar(p_clr)
  }
  k = n + 1
  res
}

# add ANSI link on input path if supported
ansi_link = function(x) {
  if (length(x) && isTRUE(as.logical(Sys.getenv('RSTUDIO_CLI_HYPERLINKS'))))
    .env$input2 = sprintf(
      '\033]8;%s;file://%s\a%s\033]8;;\a', link_pos(), normalize_path(x), x
    )
}

# use options(bitmapType = 'cairo') for bitmap devices on macOS if possible
bitmap_type = function() {
  # xquartz has to be installed for cairo to work (I don't know why)
  if (xfun::is_macos() && capabilities('cairo') && Sys.which('xquartz') != '')
    'cairo' else .Options$bitmapType
}

time_frame = function(s = NA_character_, l = integer(), lab = NA_character_, t = NA_real_) {
  data.frame(source = s, line1 = l[1], line2 = l[2], label = lab, time = t)
}

record_time = function(x, lines, label) {
  x = as.numeric(x)
  gc(FALSE)
  d = time_frame(.env$input %||% '#text', lines, label, x)
  .env$time = append(.env$time, list(d))
}

#' Get the timing data of code chunks and text blocks in a document
#'
#' Timing can be enabled via the chunk option `time = TRUE` (e.g., set
#' [litedown::reactor]`(time = TRUE)` in the first code chunk). After it is
#' enabled, the execution time for code chunks and text blocks will be recorded.
#' This function can be called to retrieve the timing data later in the document
#' (e.g., in the last code chunk).
#' @param threshold A number (time in seconds) to subset data with. Only rows
#'   with time above this threshold are returned.
#' @param sort Whether to sort the data by time in the decreasing order.
#' @param total Whether to append the total time to the data.
#' @note By default, the data will be cleared after each call of [fuse()] and
#'   will not be available outside [fuse()]. To store the data persistently, you
#'   can set the `time` option to a file path. This is necessary if you want to
#'   get the timing data for multiple input documents (such as all chapters of a
#'   book). Each document needs to point the `time` option to the same path.
#'   When you do not need timing any more, you will need to delete this file by
#'   yourself.
#' @return A data frame containing input file paths, line numbers, chunk labels,
#'   and time. If no timing data is available, `NULL` is returned.
#' @export
timing_data = function(threshold = 0, sort = TRUE, total = TRUE) {
  d = .env$time
  if (!is.null(d)) d = do.call(rbind, d)

  if (is.character(path <- timing_path())) {
    if (file_exists(path)) {
      d2 = readRDS(path)
      if (length(input <- .env$input)) d2 = subset(d2, source != input)
      d = rbind(d2, d)
    }
    saveRDS(d, path)
  }
  if (is.null(d)) return(invisible(d))

  total = if (total) sum(d$time)
  # add edit links in the roam() mode
  if (is_roaming() && !all(i <- d$source == '#text')) {
    d$source = ifelse(i, '', sprintf(
      '%s [&#9998;](?path=%s&line=%d)', d$source, URLencode(d$source, TRUE), d$line1
    ))
  }
  d = d[d$time > threshold, ]
  if (sort) d = d[order(d$time, decreasing = TRUE), ]
  if (!is.null(total)) d = rbind(d, time_frame('Total', t = total))
  d
}

timing_path = function() {
  p = xfun::env_option('litedown.time')
  if (is.character(p) && tolower(p) %in% c('true', 'false')) p = as.logical(p)
  if (is.logical(p) || (is.character(p) && p != '')) p else reactor('time')
}

# an internal function for RStudio IDE to recognize the custom knit function
# when users hit the Knit button
knit = function(input, ...) fuse(input, envir = parent.frame())

fuse_code = function(x, blocks) {
  # merge local chunk options into global options
  old = reactor(x$options); on.exit(reactor(old), add = TRUE)
  opts = reactor()

  # delayed assignment to evaluate a chunk option only when it is actually used
  lapply(setdiff(names(opts), 'order'), function(i) {
    # skip the `order` option since it needs to be eval()ed in block_order()
    if (i != 'order' && is_lang(o <- opts[[i]]))
      delayedAssign(i, eval(o, fuse_env()), environment(), opts)
  })
  # set the working directory before evaluating anything else
  change_wd(opts$wd)

  # fuse child documents (empty the `child` option to avoid infinite recursion)
  if (length(opts$child)) return(uapply(reactor(child = NULL)$child, function(.) {
    child = .env$child; .env$child = TRUE; on.exit(.env$child <- child)
    fuse(., output = 'markdown', envir = fuse_env(), quiet = TRUE)
  }))

  lab = opts$label; lang = opts$engine

  # the source code could be from chunk options 'file' or 'code'
  test_source = function(name) {
    if (length(opts[[name]]) == 0) return(FALSE)
    if (cond <- length(x$source) > 0) warning(
      "The chunk option '", name, "' is ignored for the non-empty chunk:\n\n",
      one_string(x$source)
    )
    !cond
  }
  if (test_source('file')) {
    x$source = read_all(opts$file)
  } else if (test_source('code')) {
    x$source = opts$code
  } else {
    # use code from other chunks of the same label
    labs = if (length(x$source) == 0) which(names(blocks) == lab)
    if (length(labs)) x$source = uapply(blocks[labs], `[[`, 'source')
  }

  # resolve inline chunk references and do code interpolation
  x$source = fill_source(x$source, opts$fill, blocks)

  res = if (isFALSE(opts$eval)) list(new_source(x$source)) else {
    if (is.function(eng <- engines(lang))) eng(x) else list(
      new_source(x$source),
      new_warning(sprintf("The engine '%s' is not supported.", lang))
    )
  }

  if (!opts$include) return('')

  # if the engine result contains new chunk options, apply the new options, and
  # make sure they will be properly restored via `old` on exit
  if (length(opts_new <- attr(res, 'options'))) {
    old2 = reactor(opts_new)
    for (i in setdiff(names(old2), names(old))) old[i] = old2[i]
  }

  # decide the number of backticks to wrap up output
  fence = xfun::make_fence(c(unlist(res), x$fences))

  # deal with figure alt text, captions, and environment
  env = opts$fig.env; alt = opts$fig.alt; cap = opts$fig.cap
  att = if (is.null(att <- opts$attr.plot)) '' else paste0('{', att, '}')
  if (is.null(alt)) alt = cap
  p1 = Filter(function(x) !is_plot(x), res)
  p2 = Filter(is_plot, res)
  p3 = unlist(p2)  # vector of plot paths
  # get the relative path of the plot directory
  fig.dir = if (length(p3)) tryCatch(
    sub('^[.]/', '.', paste0(dirname(relative_path(p3[1], .env$wd_out)), '/')),
    error = function(e) NULL
  )
  fig.dir[fig.dir == "."] = ""

  # record plot paths so they can be cleaned up if option embed_cleanup = true;
  # however, when cache = true, we shouldn't clean up plots since they won't be
  # regenerated next time (then they won't be found)
  if (!isTRUE(opts$cache)) .env$plot_files = c(.env$plot_files, p3)
  # recycle alt and attributes for all plots
  pn = length(p3)
  if (pn && is.null(alt)) {
    # reminder about missing alt text if this option is set to TRUE
    if (getOption('litedown.fig.alt', FALSE)) message(
      "\nPlease provide a 'fig.alt' option to the code chunk at ", get_loc(lab)
    )
    alt = ''
  }
  alt = rep(alt, length.out = pn)
  att = rep(att, length.out = pn)
  # if figure caption is provided, merge all plots in one env
  if (pn && length(cap)) res = c(xfun:::merge_record(p1), list(new_plot(p3)))
  i = 0  # plot counter

  res_show = opts$results  # normalize the 'results' option
  if (is.logical(res_show)) res_show = if (res_show) 'markup' else 'hide'

  l1 = x$code_start  # starting line number of the whole code chunk
  # generate markdown output
  out = lapply(res, function(x) {
    type = grep_sub('^record_', '', class(x))[1]
    if (is.na(type)) type = 'output'
    if (type == 'source') {
      if (!opts$echo) return()
      l2 = attr(x, 'lines')[1]  # starting line number of a code block
      x = one_string(x)
      if (opts$strip.white) x = trim_blank(x)
    }
    asis = if (type %in% c('output', 'asis')) {
      if (res_show == 'hide') return()
      any(c(res_show, type) == 'asis')
    } else FALSE
    if (type == 'warning' && !isTRUE(opts$warning)) return()
    if (type == 'message' && !isTRUE(opts$message)) return()
    if (type == 'plot') {
      n = length(x); i2 = i + seq_len(n); i <<- i + n
      img = sprintf(
        '![%s](<%s>)%s', alt[i2],
        if (is.null(fig.dir)) x else gsub('^.*/', fig.dir, x), att[i2]
      )
      add_cap(img, cap, lab, opts$cap.pos, env)
    } else {
      a = opts[[paste0('attr.', type)]]
      if (type == 'source') {
        # use engine name as class name; when `a` contains class names, prefix language-
        if (!any(grepl('(^| )[.]', a))) a = c(paste0('.',  lang), a)
        # add line numbers
        if (is_roaming()) a = c(a, lineno_attr(NA, l1 + l2 - 1))
      } else {
        if (type == 'message') x = sub('\n$', '', x)
        if (!asis) {
          opt2 = attr(x, 'opts')
          cmt = opts$comment %||% opt2$comment %||% '#> '
          if (cmt != '') {
            x = split_lines(x)
            x = paste0(cmt, x)  # comment out text output
          }
          if (is.null(a)) if (!is.null(a <- opt2$attr)) a = c(a, '.plain')
        }
      }
      if (asis) {
        if (is.null(a)) x else fenced_div(x, a)
      } else fenced_block(x, a, fence)
    }
  })
  out = dropNULL(out)

  # collapse a code block without attributes into previous adjacent code block
  # (also try to collapse for results = 'hide')
  collapse = isTRUE(opts$collapse) || res_show == 'hide'
  if (collapse && (n <- length(out)) > 1) {
    i1 = 1; k = NULL  # indices of elements to be removed from `out`
    for (i in 2:n) {
      if (i - i1 > 1) i1 = i - 1  # make sure blocks are adjacent
      o1 = out[[i1]]; n1 = length(o1); e1 = o1[n1]
      # previous block should have a closing fence ```+
      if (n1 < 3 || !grepl('^```+$', e1)) next
      o2 = out[[i]]
      if (continue_block(o1[2], e1, head(o2, 2))) {
        out[[i]] = c(o1[-n1], o2[-(1:2)])
        k = c(k, i1)  # merge previous block into current and remove previous
      }
    }
    if (length(k)) out = out[-k]
  }

  a = opts$attr.chunk
  if (length(x$fences) == 2) {
    # add a class name to the chunk output so we can style it differently
    a = c(a, '.fenced-chunk')
    out = add_fences(out, x, fence)
  }
  out = unlist(out)
  if (!is.null(a)) out = fenced_div(out, a)
  # if first line of chunk output is empty, remove it (the chunk should have had
  # an empty line before it in the source)
  if (length(out) && out[1] == '') out = out[-1]
  # add prefix (possibly indentation and > quote chars)
  if (!is.null(x$prefix)) out = gsub('^|(?<=\n)', x$prefix, out, perl = TRUE)
  out
}

# resolve `<label>` to chunk source, and evaluate `{code}` to string (to
# interpolate original source)
fill_source = function(x, fill, blocks) {
  if (isFALSE(fill)) return(x)
  x = fill_label(x, blocks)
  fill_code(x, fill)
}

fill_label = function(x, blocks) {
  r = '`<(.+?)>`'
  for (i in grep(r, x)) {
    ind = sub('^(\\s*).*', '\\1', x[i])  # possible indent
    x[i] = match_replace(x[i], r, function(z) {
      labs = sub(r, '\\1', z)  # chunk label
      j = labs %in% names(blocks)
      if (any(j)) z[j] = uapply(blocks[labs[j]], function(b) {
        s = b$source
        if ((n <- length(s)) > 0) {
          paste0(c('', rep(ind, n - 1)), s, collapse = '\n')
        } else ''
      })
      z
    })
  }
  # recursion for possible more `<label>` markers
  if (is.null(i)) x else fill_label(split_lines(x), blocks)
}

fill_code = function(x, fill) {
  r = '`\\{(.+?)}`'
  match_replace(x, r, function(z) {
    code = sub(r, '\\1', z)
    uapply(code, function(s) {
      v = eval_code(s)
      if (is.function(fill)) v = fill(v)
      one_string(v)
    })
  })
}

# temporarily change the working directory inside a function call
change_wd = function(dir) {
  if (is.character(dir)) {
    owd = setwd(dir); exit_call(function() setwd(owd))
  }
}

# add caption to an element (e.g., figure/table)
add_cap = function(x, cap, lab, pos, env, type = 'fig') {
  if (length(cap) == 0 || length(lab) == 0) return(x)
  cap = fenced_div(add_ref(lab, type, cap), '.caption')
  pos = pos %||% 'bottom'
  x = if (pos == 'top') c(cap, '', x) else c(x, '', cap)
  fenced_div(x, c(sub('^[.]?', '.', env), sprintf('#%s:%s', type, lab)))
}

# if original chunk header contains multiple curly braces (e.g., ```{{lang}}),
# include chunk fences in the output (and also pipe comments if exist)
add_fences = function(out, x, fence) {
  # remove last line of pipe comments if empty
  if ((n <- length(o <- x$comments)) > 1 && o[n] == '') o = o[-n]
  fences = list(c(x$fences[1], o), x$fences[2])
  append(lapply(fences, fenced_block, c('.md', '.code-fence'), fence), out, 1)
}

# attributes for code blocks with line numbers
lineno_attr = function(lang = NA, start = 1, auto = TRUE) c(
  if (!is.na(lang)) paste0('.', sub('^[rq]md$', 'md', tolower(lang))),
  '.line-numbers', if (auto) '.auto-numbers', sprintf('data-start="%d"', start)
)

# two blocks are continuous if first 2 elements of next block are '' and
# previous block's closing or opening fence (after removing data-start attribute)
continue_block = function(e1_open, e1_end, e2) {
  if (length(e2) != 2 || e2[1] != '') return(FALSE)
  if ((e2_open <- e2[2]) == e1_end) return(TRUE)
  e3 = sub(' data-start="[0-9]+"', '', c(e1_open, e2_open))
  e3[1] == e3[2]
}

new_source = function(x) {
  len = length(x)
  structure(new_record(x, 'source'), lines = if (len) c(1L, len) else c(0L, 0L))
}
new_output = function(x) new_record(x, 'output')
new_warning = function(x) new_record(x, 'warning')
new_plot = function(x) new_record(x, 'plot')
new_asis = function(x, raw = FALSE) {
  res = new_record(x, 'asis')
  if (raw) raw_string(res) else res
}

#' Mark a character vector as raw output
#'
#' This function should be called inside a code chunk, and its effect is the
#' same as the chunk option `results = "asis"`. The input character vector will
#' be written verbatim to the output (and interpreted as Markdown).
#' @param x A character vector (each element will be treated as a line).
#' @param format An output format name, e.g., `html` or `latex`. If provided,
#'   `x` will be wrapped in a fenced code block, e.g., ```` ```{=html}````.
#' @return A character vector with a special class to indicate that it should be
#'   treated as raw output.
#' @export
#' @examples
#' litedown::raw_text(c('**This**', '_is_', '[Markdown](#).'))
#' litedown::raw_text('<b>Bold</b>', 'html')
#' litedown::raw_text('\\textbf{Bold}', 'latex')
raw_text = function(x, format = NULL) {
  if (length(fmt <- sprintf('=%s', format)) == 1) x = fenced_block(x, fmt)
  new_asis(x, TRUE)
}

is_plot = function(x) inherits(x, 'record_plot')

fuse_text = function(x) {
  if (is.character(src <- x$source)) return(one_string(src))
  res = lapply(src, function(s) {
    if (is.character(s)) s else exec_inline(s)
  })
  unlist(res)
}

exec_inline = function(x) {
  save_pos(x$pos)
  o = reactor(x$options); on.exit(reactor(o), add = TRUE)
  opts = reactor()
  if (isFALSE(opts$eval)) return('')
  change_wd(opts$wd)
  lang = x$options$engine
  if (is.function(eng <- engines(lang))) {
    one_string(eng(x, inline = TRUE))
  } else {
    warning("The inline engine '", lang, "' is not supported.")
    sprintf('`{%s} %s`', lang, x$source)
  }
}

fmt_inline = function(x, ...) {
  if (is.numeric(x) && length(x) == 1 && !inherits(x, 'AsIs'))
    sci_num(x, ...) else as.character(x)
}

# change scientific notation to LaTeX math
sci_num = function(x, math = NULL) {
  opts = reactor()
  s = x != 0 && abs(log10(abs(x))) >= opts$power
  x = format(signif(x, opts$signif), scientific = s)
  r = '^(-)?([0-9.]+)e([-+])0*([0-9]+)$'
  s = grepl(r, x)
  if (s) {
    n = match_one(x, r)[[1]]
    x = sprintf(
      '%s%s10^{%s%s}', n[2], if (n[3] == '1') '' else paste(n[3], '\\times '),
      if (n[4] == '+') '' else n[4], n[5]
    )
  }
  if (is.na(d <- opts$dollar)) d = s && !isTRUE(math)
  if (d) paste0('$', x, '$') else x
}

# similar to the base R options() interface but for litedown options / engines /
# ..., and is based on environments, which are *mutable*
new_opts = function() {
  # global chunk options
  .opts = structure(new_env(), class = c('litedown_env', 'environment'))

  opt_get = function(x, drop = length(x) == 1) {
    vs = mget(x, .opts, ifnotfound = list(NULL))
    if (drop) vs[[1]] else vs
  }
  # setter: fun(opt = val); getter: fun('opt')
  function(...) {
    v = list(...)
    n = length(v)
    if (n == 0) return(.opts)
    if (is.null(nms <- names(v))) {
      if (all(vapply(v, is.character, TRUE))) return(opt_get(unlist(v)))
      if (n > 1) warning(
        'When not all unnamed arguments are character, only the first argument is used (',
        n, ' were received).'
      )
      if (is.null(nms <- names(v <- v[[1]])) || any(nms == '')) stop(
        'When the first unnamed argument is not character, it must be a named list.'
      )
    }
    if (any(nms == '')) stop('All arguments must be either named or unnamed.')
    old = opt_get(nms, drop = FALSE)
    for (i in nms) assign(i, v[[i]], envir = .opts)
    invisible(old)
  }
}

#' Get and set chunk options
#'
#' Chunk options are stored in an environment returned by `reactor()`. Option
#' values can be queried by passing their names to `reactor()`, and set by
#' passing named values.
#' @param ... Named values (for setting) or unnamed values (for getting).
#' @return With no arguments, `reactor()` returns an environment that stores the
#'   options, which can also be used to get or set options. For example, with
#'   `opts = reactor()`, `opts$name` returns an option value, and `opts$name =
#'   value` sets an option to a value.
#'
#'   With named arguments, `reactor()` sets options and returns a list of their
#'   old values (e.g., `reactor(echo = FALSE, fig.width = 8)`). The returned
#'   list can be passed to `reactor()` later to restore the options.
#'
#'   With unnamed arguments, `reactor()` returns option values after received
#'   option names as input. If one name is received, its value is returned
#'   (e.g., `reactor('echo')`). If multiple names are received, a named list of
#'   values is returned (e.g., `reactor(c('echo', 'fig.width'))`). A special
#'   case is that if only one unnamed argument is received and it takes a list
#'   of named values, the list will be used to set options, e.g.,
#'   `reactor(list(echo = FALSE, fig.width = 8))`, which is equivalent to
#'   `reactor(echo = FALSE, fig.width = 8)`.
#' @export
#' @examples
#' # get options
#' litedown::reactor('echo')
#' litedown::reactor(c('echo', 'fig.width'))
#'
#' # set options
#' old = litedown::reactor(echo = FALSE, fig.width = 8)
#' litedown::reactor(c('echo', 'fig.width'))
#' litedown::reactor(old)  # restore options
#'
#' # use the environment directly
#' opts = litedown::reactor()
#' opts$echo
#' mget(c('echo', 'fig.width'), opts)
#' ls(opts)  # built-in options
reactor = new_opts()
reactor(
  eval = TRUE, echo = TRUE, fill = TRUE, results = TRUE, comment = NULL,
  warning = TRUE, message = TRUE, error = NA, include = TRUE,
  strip.white = TRUE, collapse = FALSE, order = 0,
  attr.source = NULL, attr.output = NULL, attr.plot = NULL, attr.chunk = NULL,
  attr.message = '.plain .message', attr.warning = '.plain .warning', attr.error = '.plain .error',
  cache = FALSE, cache.path = NULL,
  dev = NULL, dev.args = NULL, fig.path = NULL, fig.ext = NULL,
  fig.width = 7, fig.height = 7, fig.dim = NULL, fig.cap = NULL, fig.alt = NULL, fig.env = '.figure',
  tab.cap = NULL, tab.env = '.table', cap.pos = NULL,
  print = NULL, print.args = NULL, time = FALSE,
  code = NULL, file = NULL, child = NULL, purl = TRUE,
  wd = NULL,
  signif = 3, power = 6, dollar = NA
)

eval_code = function(code, error = NA) {
  expr = parse_only(code)
  if (is.na(error)) eval(expr, fuse_env()) else tryCatch(
    eval(expr, fuse_env()), error = function(e) if (error) e$message else ''
  )
}

# the R engine
eng_r = function(x, inline = FALSE, ...) {
  opts = reactor()
  if (inline) {
    res = eval_code(x$source, opts$error)
    return(fmt_inline(res, x$math))
  }
  args = reactor(
    'fig.path', 'fig.ext', 'dev', 'dev.args', 'message', 'warning', 'error',
    'cache', 'print', 'print.args', 'verbose'
  )
  if (is.character(args$fig.path)) args$fig.path = paste0(args$fig.path, opts$label)
  size = list(width = opts$fig.width, height = opts$fig.height)
  # if fig.dim is provided, override fig.width and fig.height
  if (length(dm <- opts$fig.dim) == 2) size[] = as.list(dm)
  # map chunk options to record() argument names
  names(args)[1:2] = c('dev.path', 'dev.ext')
  args = dropNULL(args)
  args$dev.args = merge_list(size, opts$dev.args)
  args$cache = list(
    path = if (args$cache) opts$cache.path, vars = opts$cache.vars,
    hash = opts$cache.hash, extra = opts$cache.extra, keep = opts$cache.keep,
    id = opts$label, rw = opts$cache.rw
  )
  do.call(xfun::record, c(list(code = x$source, envir = fuse_env()), args))
}

# the Markdown engine: echo Markdown source verbatim, and also output it as-is
eng_md = function(x, inline = FALSE, ...) {
  s = x$source
  if (inline) {
    f = unlist(match_all(s, '`+'))  # how many backticks to quote the text?
    f = if (length(f)) paste0('`', max(f)) else '`'
    one_string(c(f, s, f, s), ' ')
  } else list(new_source(s), new_asis(s))
}

# the Mermaid engine: put code in ```mermaid and add caption if necessary
eng_mermaid = function(x, inline = FALSE, ...) {
  code = fenced_block(src <- x$source, 'mermaid')
  opts = reactor()
  code = add_cap(code, opts$fig.cap, opts$label, opts$cap.pos, opts$fig.env)
  if (inline) one_string(c(code, '')) else {
    list(new_source(src), new_asis(code))
  }
}

# embed a file verbatim
eng_embed = function(x, ...) {
  s = x$source; opts = reactor()
  # if chunk option `file` is empty, use source code as the list of files
  if (is.null(f <- opts$file)) {
    f = gsub('^["\']|["\']$', '', s)  # in case paths are quoted
    if (length(f) == 0) return()
    s = read_all(f)
  }
  opts_new = list(comment = '')  # don't comment out file content
  # use the filename extension as the default language name
  if (is.null(opts$attr.output) && nchar(lang <- file_ext(f[1])) > 1) {
    lang = sub('^R', '', lang)  # Rmd -> md, Rhtml -> html, etc.
    if (lang == 'nw') lang = 'tex'
    opts_new$attr.output = paste0('.', lang)
  }
  structure(list(new_output(s)), options = opts_new)
}

eng_html = function(x, inline = FALSE, html = NULL) {
  out = fenced_block(html, '=html')
  if (inline) one_string(c(out, '')) else list(new_source(x$source), new_asis(out))
}

eng_css = function(x, inline = FALSE, ...) {
  if (is.character(h <- reactor('href'))) {
    res = sprintf('<link rel="stylesheet" href="%s">', h)
    if (inline) one_string(res) else list(new_asis(res))
  } else {
    eng_html(x, inline, c('<style type="text/css">', x$source, '</style>'))
  }
}

eng_js = function(x, inline = FALSE, ...) {
  opts = reactor()
  a = list(type = opts$type, src = opts$src)
  for (i in c('type', 'src')) a[[i]] = a[[i]]  # remove NULL
  if (is.character(s <- a$src)) {
    if (!isFALSE(opts$defer)) a['defer'] = list(NULL)
    res = html_tag('script', NULL, a)
    if (inline) one_string(res) else list(new_asis(res))
  } else {
    if (identical(a$type, 'module')) a$defer = NULL
    eng_html(x, inline, html_tag('script', html_value(x$source), a))
  }
}

#' Language engines
#'
#' Get or set language engines for evaluating code chunks and inline code.
#'
#' An engine function should have three arguments:
#'
#' - `x`: An element in the [crack()] list (a code chunk or a text block).
#'
#' - `inline`: It indicates if `x` is from a code chunk or inline code.
#'
#' - `...`: Currently unused but recommended for future compatibility (more
#'   arguments might be passed to the function).
#'
#' The function should return a character value.
#' @inheritParams reactor
#' @return The usage is similar to [reactor()]: `engines('LANG')` returns an
#'   engine function for the language `LANG`, and `engines(LANG = function(x,
#'   inline = FALSE, ...) {})` sets the engine for a language.
#' @export
#' @examples
#' litedown::engines()  # built-in engines
engines = new_opts()
engines(
  r = eng_r, md = eng_md, mermaid = eng_mermaid, embed = eng_embed,
  css = eng_css, js = eng_js
)

#' @export
print.litedown_env = function(x, ...) {
  str(as.list(x, all.names = TRUE, sorted = TRUE), ...)
  invisible(x)
}

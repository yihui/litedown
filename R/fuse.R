new_env = function(...) new.env(..., parent = emptyenv())

# an internal environment to store some intermediate data
.env = new_env()

#' Parse an R Markdown document
#'
#' Parse an Rmd document into code chunks, inline code expressions, and text
#' fragments.
#'
#' A code chunk must start with a fence of the form ```` ```{lang} ````, where
#' `lang` is the language name, e.g., `r` or `python`. The body of a code chunk
#' can start with chunk options written in "pipe comments", e.g., `#| eval =
#' TRUE, echo = FALSE` (the CSV syntax) or `#| eval: true` (the YAML syntax).
#'
#' An inline code fragment is of the form `` `{lang} source` `` embedded in
#' Markdown text.
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
#'
#'   If any code chunks have labels (specified via the chunk option `label`),
#'   the whole returned list will be named using the labels.
#' @examples
#' library(litedown)
#' res = parse_rmd(c('```{r}\n1+1\n```', 'Hello, `pi` = `{r} pi` and `e` = `{r} exp(1)`!'))
#' str(res)
#' # evaluate inline code and combine results with text fragments
#' txt = lapply(res[[2]]$source, function(x) {
#'   if (is.character(x)) x else eval(parse(text = x$source))
#' })
#' paste(unlist(txt), collapse = '')
parse_rmd = function(input = NULL, text = NULL) {
  text = read_input(input, text); input = attr(text, 'input')
  xml = commonmark::markdown_xml(text, sourcepos = TRUE)
  rx_engine = '([a-zA-Z0-9_]+)'  # only allow these characters for engine names
  r = paste0(
    '<(code|code_block) sourcepos="(\\d+):(\\d+)-(\\d+):(\\d+)"( info="[{]+',
    rx_engine, '[^"]*?[}]")? xml:space="[^>]*>([^<]*)<'
  )
  m = regmatches(xml, gregexec(r, xml, perl = TRUE))[[1]]

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

  # code blocks must have non-empty info strings
  if (!length(m) || !length(m <- m[, m[2, ] != 'code_block' | m[9, ] != '', drop = FALSE]))
    return(res)

  m = m[, m[2, ] == 'code', drop = FALSE]
  # find out inline code `{lang} expr`
  rx_inline = '^\\s*[{](.+?)[}]\\s+(.+?)\\s*$'
  # look for `r expr` if `{lang}` not found (for compatibility with knitr)
  if (!any(j <- grepl(rx_inline, m[9, ])) && getOption('litedown.enable.knitr_inline', FALSE)) {
    rx_inline = '^(r) +(.+?)\\s*$'
    j = grepl(rx_inline, m[9, ])
  }
  m = m[, j, drop = FALSE]
  n_start = unlist(lapply(res, function(x) x$lines[1]))  # starting line numbers
  j = findInterval(m[3, ], n_start)  # find which block each inline code belongs to
  for (i in seq_len(ncol(m))) {
    pos = as.integer(m[3:6, i]); i1 = pos[1]; i2 = pos[3]
    b = res[[j[i]]]; l = b$lines
    # calculate new position of code after we concatenate all lines of this block by \n
    s = nchar(b$source)
    b$col = c(b$col, c(
      sum(s[seq_len(i1 - l[1])] + 1) + pos[2],
      sum(s[seq_len(i2 - l[1])] + 1) + pos[4]
    ))
    b$pos = c(b$pos, pos)
    res[[j[i]]] = b
  }

  opts = options(xfun.handle_error.loc_fun = get_loc)
  oenv = as.list(.env)
  on.exit({ options(opts); reset_env(oenv, .env) }, add = TRUE)
  .env$input = input  # store the input name for get_loc()

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
      o = regmatches(code[1], regexec(rx_opts, code[1]))[[1]]
      if (length(o)) {
        # if two or more `{` is used, we will write chunk fences to output
        if (nchar(o[3]) > 1) b$fences = c(
          sub('{{', '{', sub('}}\\s*$', '}', code[1]), fixed = TRUE), code[N]
        )
        o = if (o[5] != '') csv_options(o[5])
      }
      code = code[-c(1, N)]  # remove fences
      save_pos(b$lines)
      code = xfun::divide_chunk(b$info, code)
      b[c('source', 'options', 'comments')] = code[c('code', 'options', 'src')]
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
      # position of code c(row1, col1, row2, col2)
      pos = matrix(b$pos, nrow = 4)
      x = head(c(rbind(x2, c(x1, ''))), -1)
      x = lapply(seq_along(x), function(i) {
        z = x[i]
        if (i %% 2 == 1) return(z)
        z = regmatches(z, regexec(rx_inline, z))[[1]][-1]
        p2 = pos[, i / 2]; save_pos(p2)
        list(
          source = z[2], pos = p2,
          options = csv_options(gsub('^([^,]+)', 'engine="\\1"', z[1]))
        )
      })
      b$source = x
    } else {
      b$source = paste(b$source, collapse = '\n')
    }
    b$pos = b$col = NULL  # positions not useful anymore
    res[[j]] = b
  }
  nms = vapply(res, function(x) x$options[['label']] %||% '', character(1))
  if (!all(nms == '')) names(res) = nms
  # TODO: should we support inline chunk references? If we do, I'd prefer a new
  # syntax, e.g., `${label}`, instead of knitr's <<label>> syntax

  res
}

# convert knitr's inline `r code` to litedown's `{r} code`
convert_knitr = function(input) {
  x = read_utf8(input)
  r = '(?<!(^``))(?<!(\n``))`r[ #]([^`]+)\\s*`'
  i = prose_index(x)
  x[i] = gsub(r, '`{r} \\3`', x[i], perl = TRUE)
  write_utf8(x, input)
}

# return a string to indicate the error location
get_loc = function(label) {
  l = .env$source_pos; n = length(l)
  if (n == 4) l = sprintf('%d:%d-%d:%d', l[1], l[2], l[3], l[4])  # row1:col1-row2:col2
  if (n == 2) l = sprintf('%d-%d', l[1], l[2])  # row1-row2
  paste0(l, label, if (label == '') ' ', sprintf('(%s)', .env$input))
}

# save line numbers in .env to be used in error messages
save_pos = function(x) .env$source_pos = x

# get the execution order of code/text blocks via the `order` option (higher
# values indicate higher priority)
block_order = function(res) {
  check = function(b) {
    if (is.null(o <- b$options[['order']]) || length(o) == 1) o else stop(
      "The chunk option 'order' must be either NULL or of length 1. ",
      sprintf("Check lines %d-%d", b$lines[1], b$lines[2]),
      sprintf(" (%s)", .env$input), "."
    )
  }
  x = lapply(res, function(b) {
    if (b$type == 'code_chunk') return(check(b) %||% 0)
    if (!is.list(b$source)) return(0)
    for (s in b$source)
      if (is.list(s) && !is.null(o <- check(s))) return(o)
    0
  })
  order(unlist(x), decreasing = TRUE)
}

#' @description The function `fuse()` extracts and runs code from code chunks
#'   and inline code expressions in R Markdown, and interweaves the results with
#'   the rest of text in the input, which is similar to what [knitr::knit()] and
#'   [rmarkdown::render()] do. The function `fiss()` extracts code from the
#'   input, and is similar to [knitr::purl()].
#' @rdname mark
#' @param envir An environment in which the code is to be evaluated.
#' @param quiet If `TRUE`, do not show the progress bar. If `FALSE`, the
#'   progress bar will be shown after a number of seconds, which can be set via
#'   a global [option][options] `litedown.progress.delay` (the default is `2`).
#'   THe progress bar output can be set via a global option
#'   `litedown.progress.output` (the default is [stderr()]).
#' @export
#' @examples
#' library(litedown)
#' doc = c('```{r}', '1 + 1', '```', '', '$\\pi$ = `{r} pi`.')
#' fuse(doc)
#' fuse(doc, '.tex')
#' fiss(doc)
fuse = function(input, output = NULL, text = NULL, envir = parent.frame(), quiet = FALSE) {
  text = read_input(input, text); input = attr(text, 'input')
  yaml = yaml_body(text)$yaml
  format = detect_format(output, yaml)
  output = auto_output(input, output, format)
  output_base = if (is_output_file(output)) sans_ext(output)

  # cleaning up some objects on exit
  opts = reactor(); on.exit(reactor(opts), add = TRUE)
  if (chunk_counter$get() == 0) on.exit(chunk_counter$reset(), add = TRUE)
  oenv = as.list(.env); on.exit(reset_env(oenv, .env), add = TRUE)

  # set working directory if unset
  if (is_file(input) && is.null(opts$wd)) opts$wd = dirname(normalizePath(input))

  # store output dir so we can calculate relative paths for plot files later
  .env$wd.out = xfun::normalize_path(
    if (is.null(output_base)) {
      if (is.character(.env$wd.out)) .env$wd.out else '.'
    } else dirname(output_base)
  )

  # set default device to 'cairo_pdf' for LaTeX output, and 'png' for other formats
  if (is.null(opts$dev)) {
    opts$dev = if (format == 'latex') 'cairo_pdf' else 'png'
  }
  # set default figure and cache paths
  set_path = function(name) {
    # fig.path = output-files/ if `output` is a path, otherwise use
    # litedown-files/ (we don't use *_files because of rstudio/rmarkdown#2550)
    if (is.null(p <- opts[[name]])) p = paste0(
      output_base %||% 'litedown', c(fig.path = '-files/', cache.path = '-cache/')[name]
    )
    # make sure path is absolute so it will be immune to setwd() (in code chunks)
    if (xfun::is_rel_path(p)) p = file.path(getwd(), p)
    opts[[name]] = p
  }
  set_path('fig.path'); set_path('cache.path')
  # clean up the figure folder on exit if it's empty
  on.exit(xfun::del_empty_dir({
    if (dir.exists(fig.dir <- opts$fig.path)) fig.dir else dirname(fig.dir)
  }), add = TRUE)

  blocks = parse_rmd(input, text)
  .env$input = input
  res = .fuse(blocks, input, envir, quiet)

  # keep the markdown output if keep_md = TRUE is set in YAML output format
  if (is_output_file(output) && isTRUE(yaml_field(yaml, format, 'keep_md'))) {
    write_utf8(res, with_ext(output, '.md'))
  }

  # if output = '.md' or 'markdown', no need for further mark() conversion
  if (is.character(output) && grepl('[.]md$|^markdown$', output)) {
    if (is_output_file(output) && output != 'markdown') {
      write_utf8(res, output)
    } else raw_string(res)
  } else {
    mark(input, output, res)
  }
}

#' @rdname mark
#' @export
fiss = function(input, output = '.R', text = NULL) {
  text = read_input(input, text); input = attr(text, 'input')
  output = auto_output(input, output, NULL)
  blocks = parse_rmd(input, text)
  # TODO: what should we do for non-R code? also consider eval=FALSE and error=TRUE
  res = unlist(lapply(blocks, function(b) {
    if (b$type == 'code_chunk' && !isFALSE(b$options$purl) && b$options$engine == 'r')
      c(b$source, '')
  }))
  if (is_output_file(output)) write_utf8(res, output) else raw_string(res)
}

.fuse = function(blocks, input, envir, quiet) {
  n = length(blocks)

  # a simple progress indicator
  p_lab = names(blocks) %||% rep('', n)  # labels to be displayed in progress
  p_lab = ifelse(p_lab == '', '', sprintf(' [%s] ', p_lab))
  p_len = max(c(0, nchar(p_lab))) + 5  # 5 == nchar('100% ')
  p_clr = paste0('\r', strrep(' ', p_len), '\r')  # a string to clear the progress
  p_out = getOption('litedown.progress.output', stderr())
  t0 = Sys.time(); td = getOption('litedown.progress.delay', 2)

  # the chunk option `order` determines the execution order of chunks
  o = block_order(blocks)
  res = character(n)
  for (i in seq_len(n)) {
    k = o[i]; b = blocks[[k]]; save_pos(b$lines)
    res[k] = xfun:::handle_error(
      if (b$type == 'code_chunk') {
        one_string(fuse_code(b, envir, blocks))
      } else {
        one_string(fuse_text(b, envir), '')
      },
      function(e, loc) sprintf('Quitting from lines %s', loc),
      p_lab[k], get_loc
    )
    if (!quiet && Sys.time() - t0 > td) cat(
      p_clr, if (i < n) c(p_lab[k], as.character(round(i/n * 100)), '%'),
      sep = '', file = p_out
    )
  }
  res
}

# an internal function for RStudio IDE to recognize the custom knit function
# when users hit the Knit button
knit = function(input, ...) fuse(input, envir = parent.frame())

fuse_code = function(x, envir, blocks) {
  # merge local chunk options into global options
  old = reactor(x$options); on.exit(reactor(old), add = TRUE)
  opts = reactor()

  # evaluate `wd` for now to make sure we set the right working directory before
  # evaluating anything else
  opts$wd = eval_lang(opts$wd, envir)
  if (is.character(opts$wd)) {
    owd = setwd(opts$wd); on.exit(setwd(owd), add = TRUE)
  }
  for (i in names(opts)) opts[[i]] = eval_lang(opts[[i]], envir)

  # fuse child documents (empty the `child` option to avoid infinite recursion)
  if (length(opts$child)) return(unlist(lapply(reactor(child = NULL)$child, function(.) {
    child = .env$child; .env$child = TRUE; on.exit(.env$child <- child)
    fuse(., output = NA, format = 'markdown', envir = envir, quiet = TRUE)
  })))

  # the source code could be from these chunk options: file, code, or ref.label
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
  } else if (test_source('ref.label')) {
    x$source = unlist(lapply(blocks[opts$ref.label], `[[`, 'source'))
  }

  args = reactor('fig.path', 'fig.ext', 'dev', 'dev.args', 'error')
  # map chunk options to record() argument names
  names(args)[1:2] = c('dev.path', 'dev.ext')
  args = dropNULL(args)
  lab = opts$label
  args$dev.path = paste0(args$dev.path, lab)
  args$dev.args = merge_list(
    list(width = opts$fig.width, height = opts$fig.height), opts$dev.args
  )

  lang = opts$engine
  res = if (opts$eval) {
    if (lang == 'r') {
      do.call(xfun::record, c(list(code = x$source, envir = envir), args))
    } else if (is.function(eng <- engines(lang))) eng(x) else {
      warning("The engine '", lang, "' is not supported yet.")
      return('')
    }
  } else {
    list(new_source(x$source))
  }

  if (!opts$include) return('')

  # decide the number of backticks to wrap up output
  fence = xfun::make_fence(c(unlist(res), x$fences))

  # deal with figure alt text, captions, and environment
  env = opts$fig.env; alt = opts$fig.alt; cap = opts$fig.cap
  att = if (is.null(att <- opts$attr.plot)) '' else paste0('{', att, '}')
  if (is.null(alt)) alt = cap
  if (is.null(alt)) alt = ''
  p1 = Filter(function(x) !is_plot(x), res)
  p2 = Filter(is_plot, res)
  # get the relative path of the plot directory
  fig.dir = if (length(p2)) tryCatch(
    sub('^[.]/', '.', paste0(dirname(xfun::relative_path(p2[[1]][1], .env$wd.out)), '/')),
    error = function(e) NULL
  )

  # recycle alt and attributes for all plots
  pn = length(unlist(p2))
  alt = rep(alt, length.out = pn)
  att = rep(att, length.out = pn)
  # if figure env is provided, merge all plots in one env
  if (!is.null(env)) res = c(p1, new_plot(unlist(p2)))
  i = 0  # plot counter

  # generate markdown output
  out = lapply(res, function(x) {
    type = sub('record_', '', class(x))
    if (type == 'source') {
      if (!opts$echo) return()
      x = one_string(x)
      if (opts$strip.white) x = str_trim(x)
    }
    if (type %in% c('output', 'asis')) {
      if (opts$results == 'hide') return()
      if (any(c(opts$results, type) == 'asis')) return(x)
    }
    if (type == 'warning' && !opts$warning) return()
    if (type == 'message' && !opts$message) return()
    if (type == 'plot') {
      n = length(x); i2 = i + seq_len(n); i <<- i + n
      img = sprintf(
        '![%s](<%s>)%s', alt[i2],
        if (is.null(fig.dir)) x else gsub('^.*/', fig.dir, x), att[i2]
      )
      if (is.null(env)) img else fenced_block(
        c(img, '', sprintf('<span>#fig:%s</span> %s', lab, cap)), env, char = ':'
      )
    } else {
      a = opts[[paste0('attr.', type)]]
      if (type == 'source') {
        a = c(paste0('.', lang), a)  # use engine name as class name
      } else {
        x = paste0(opts$comment, x)  # comment out text output
      }
      fenced_block(x, a, fence)
    }
  })
  a = opts$attr.chunk
  if (length(x$fences) == 2) {
    # add a class name to the chunk output so we can style it differently
    a = c(a, '.fenced-chunk')
    out = add_fences(out, x, fence)
  }
  out = unlist(out)
  if (!is.null(a)) out = fenced_block(out, a, char = ':')
  if (!is.null(x$prefix)) out = paste0(x$prefix, out)
  out
}

# if original chunk header contains multiple curly braces (e.g., ```{{lang}}),
# include chunk fences in the output (and also pipe comments if exist)
add_fences = function(out, x, fence) {
  fences = list(c(x$fences[1], x$comments), x$fences[2])
  append(lapply(fences, fenced_block, c('.md', '.code-fence'), fence), out, 1)
}

new_record = function(x, class) structure(x, class = paste0('record_', class))
new_source = function(x) new_record(x, 'source')
new_output = function(x) new_record(x, 'output')
new_plot   = function(x) new_record(x, 'plot')

is_plot = function(x) inherits(x, 'record_plot')

fuse_text = function(x, envir) {
  if (is.character(src <- x$source)) return(src)
  res = lapply(src, function(s) {
    if (is.character(s)) s else exec_inline(s, envir)
  })
  unlist(res)
}

exec_inline = function(x, envir) {
  save_pos(x$pos)
  o = x$options
  if (o$engine != 'r') {
    warning("The inline engine '", o$engine, "' is not supported yet")
    return('')
  }
  res = eval(xfun::parse_only(x$source), envir)
  # TODO: allow for custom coercion functions here
  as.character(res)
}

# similar to the base R options() interface but for litedown options / engines /
# ..., and is based on environments, which are *mutable*
new_opts = function() {
  # global chunk options
  .opts = structure(new_env(), class = 'litedown_env')

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
#' @param ... Named values (for setting options) or unnamed values (for getting
#'   options).
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
  eval = TRUE, echo = TRUE, results = 'markup', comment = '#> ',
  warning = TRUE, message = TRUE, error = FALSE, include = TRUE,
  strip.white = TRUE, order = 0,
  attr.source = NULL, attr.output = NULL, attr.plot = NULL, attr.chunk = NULL,
  attr.message = '.plain .message', attr.warning = '.plain .warning', attr.error = '.plain .error',
  cache = FALSE, cache.path = NULL,  # TODO: cache not implemented
  dev = NULL, dev.args = NULL, fig.path = NULL, fig.ext = NULL,
  fig.width = 7, fig.height = 7, fig.cap = NULL, fig.alt = NULL, fig.env = NULL,
  code = NULL, file = NULL, ref.label = NULL, child = NULL, purl = TRUE,
  wd = NULL
)

# language engines
engines = new_opts()
engines(
  css = function(x) eng_html(x, '<style type="text/css">', '</style>'),
  js = function(x) eng_html(x, '<script>', '</script>')
)

eng_html = function(x, before = NULL, after = NULL) {
  out = fenced_block(c(before, x$source, after), '=html')
  list(new_source(x$source), new_record(out, 'asis'))
}

#' @export
print.litedown_env = function(x, ...) {
  str(as.list.environment(x, all.names = TRUE, sorted = TRUE), ...)
  invisible(x)
}

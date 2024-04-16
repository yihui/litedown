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
#' An inline code fragment is of the form `` `{lang} code` `` embedded in
#' Markdown text.
#' @param input The input file.
#' @param text A character vector of the Rmd source. If provided, the `input`
#'   argument will be ignored.
#' @export
#' @return A list of code chunks and text blocks:
#'
#'   Code chunks are of the form `list(source, type = "code_chunk", options,
#'   comments, ...)`: `source` is a character vector of the source code of a
#'   code chunk, `options` is a list of chunk options, and `comments` is a
#'   vector of pipe comments.
#'
#'   Text blocks are of the form `list(source, type = "text_block", ...)`. If
#'   the text block does not contain any inline code, `source` will be a
#'   character string (lines of text concatenated by line breaks), otherwise it
#'   will be a list with members that are either character strings (normal text
#'   fragments) or lists of the form `list(code, options)` (`code` is the inline
#'   code, and `options` contains its options specified inside `` `{lang, ...}`
#'   ``).
#'
#'   Both code chunks and text blocks have a list member named `lines` that
#'   stores their line numbers in the input.
#'
#'   If any code chunks have labels (specified via the chunk option `label`),
#'   the whole returned list will be named using the labels.
#' @examples
#' res = litedown::parse_rmd(text = c('```{r}\n1+1\n```', 'Hello, `pi` = `{r} pi` and `e` = `{r} exp(1)`!'))
#' str(res)
#' # evaluate inline code and combine results with text fragments
#' txt = lapply(res[[2]]$source, function(x) {
#'   if (is.character(x)) x else eval(parse(text = x$code))
#' })
#' paste(unlist(txt), collapse = '')
parse_rmd = function(input = NULL, text = xfun::read_utf8(input)) {
  if (!missing(text)) text = xfun::split_lines(text)
  xml = commonmark::markdown_xml(text, sourcepos = TRUE)
  # TODO: should we support chunk options in chunk header? (for now only in body)
  r = '<(code|code_block) sourcepos="(\\d+):(\\d+)-(\\d+):(\\d+)"( info="[{]([a-zA-Z0-9_]+)[}]")? xml:space="[^>]*>([^<]*)<'
  m = regmatches(xml, gregexec(r, xml, perl = TRUE))[[1]]

  res = list()
  # add a block of text and the line range info
  add_block = function(p, ...) {
    res[[length(res) + 1]] <<- list(source = text[p], ..., lines = p)
    res
  }
  # code blocks must have non-empty info strings
  if (!length(m) || !length(m <- m[, m[2, ] != 'code_block' | m[9, ] != '', drop = FALSE]))
    return(add_block(seq_along(text), type = 'text_block'))

  i1 = i2 = 1  # the start/end index of text blocks
  n = ncol(m)  # total number of matches
  for (i in seq_len(n)) {
    type = m[2, i]  # code or code_block
    pos  = as.integer(m[3:6, i])  # position: row1, col1, row2, col2
    info = m[8, i]  # info string (e.g., language name '{r}')
    code = m[9, i]  # source code
    if (type == 'code_block') {
      add_block(pos[1]:pos[3], info = info, type = 'code_chunk')
      i1 = pos[3] + 1  # next line of current code block is start of next text block
    } else {
      i3 = which(m[2, ] == 'code_block')
      i3 = i3[i3 > i][1]  # find the next code block
      # the previous line of next code block is the end of current text block
      i2 = if (is.na(i3)) length(text) else as.integer(m[3, i3]) - 1
      # add the text block when reaching the end of matches or the next match is code block
      if (i == n || (!is.na(i3) && i3 - i == 1)) {
        add_block(i1:i2, type = 'text_block')
      }
    }
  }

  # find out inline code `{lang} expr`
  rx_inline = '^[{](.+?)[}]\\s+(.+)'
  m = m[, m[2, ] == 'code' & grepl(rx_inline, m[9, ]), drop = FALSE]
  for (i in seq_len(ncol(m))) {
    pos = as.integer(m[3:6, i]); i1 = pos[1]; i2 = pos[3]
    for (j in seq_along(res)) {
      b = res[[j]]; l = b$lines
      # find the text block that this code belongs to
      if (i1 %in% l) {
        # calculate new position of code after we concatenate all lines of this block by \n
        s = nchar(b$source)
        b$pos = c(b$pos, c(
          sum(s[seq_len(i1 - l[1])] + 1) + pos[2],
          sum(s[seq_len(i2 - l[1])] + 1) + pos[4]
        ))
        res[[j]] = b
        break
      }
    }
  }

  opts = options('xfun.handle_error.loc_fun')
  on.exit(options(opts), add = TRUE)

  # remove code fences, and extract code in text blocks
  for (j in seq_along(res)) {
    b = res[[j]]
    options(xfun.handle_error.loc_fun = get_loc(NULL, input, b$lines))
    if (b$type == 'code_chunk') {
      code = b$source
      N = length(code)
      # a code block may be indented or inside a blockquote
      p = xfun::grep_sub('^([\t >]*)(`{3,}|~{3,}).*', '\\1\\2', code[1])
      if (length(p) == 0) stop('Possibly malformed code block fence: ', code[1])
      if (!grepl(sub('^[\t >]*', '', p), code[N])) stop(
        'The fences of the code block do not match:\n\n', code[1], '\n', code[N]
      )
      code = code[-c(1, N)]  # remove fences
      p = gsub('[`~]+$', '', p)
      i = startsWith(code, p)
      code[i] = substr(code[i], nchar(p) + 1, nchar(code[i]))  # remove indentation or >
      if (p != '') b$prefix = p
      # trailing spaces in the prefix may have been trimmed: yihui/knitr#1446
      code[!i] = gsub(gsub('(.+?)\\s+$', '^\\1', p), '', code[!i])
      code = xfun::divide_chunk(b$info, code)
      b[c('source', 'options', 'comments')] = code[c('code', 'options', 'src')]
      b$options$engine = b$info
      b$info = NULL  # the info is stored in chunk options as `engine`
    } else if (length(p <- b$pos) > 0) {
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
      x = head(c(rbind(x2, c(x1, ''))), -1)
      x = lapply(seq_along(x), function(i) {
        z = x[i]
        if (i %% 2 == 1) return(z)
        z = regmatches(z, regexec(rx_inline, z))[[1]][-1]
        list(
          code = z[2],
          options = xfun::csv_options(gsub('^([^,]+)', 'engine="\\1"', z[1]))
        )
      })
      b$source = x
    } else {
      b$source = paste(b$source, collapse = '\n')
    }
    b$pos = NULL  # position not useful anymore
    res[[j]] = b
  }
  nms = vapply(res, function(x) x$options[['label']] %||% '', character(1))
  if (!all(nms == '')) names(res) = nms

  res
}

# return a string to point out the error location
get_loc = function(block, input = NULL, lines = block$lines) {
  function(label) {
    l = if (length(lines) > 0) paste(range(lines), collapse = '-')
    paste0(l, label, if (label == '') ' ', sprintf('(%s)', input))
  }
}

# fuse code with text
fuse = function(
  input = NULL, output = NULL, text = xfun::read_utf8(input),
  format = c('html', 'latex', 'markdown'), template = TRUE,
  envir = parent.frame(), quiet = FALSE
) {
  fuse_wrapper(
    input, output, text, format[1], template, '.md', .fuse, input, format[1], envir, quiet
  )
}

.fuse = function(blocks, input, format, envir, quiet) {
  n = length(blocks)

  # a simple progress indicator
  p_lab = names(blocks) %||% rep('', n)  # labels to be displayed in progress
  p_lab = ifelse(p_lab == '', '', sprintf(' [%s] ', p_lab))
  p_len = max(c(0, nchar(p_lab))) + 5  # 5 == nchar('100% ')
  p_clr = paste0('\r', strrep(' ', p_len), '\r')  # a string to clear the progress
  t0 = Sys.time(); td = getOption('litedown.progress.delay', 2)
  p_bar = function(i, n) {
    if (quiet || Sys.time() - t0 < td) return()
    cat(p_clr, if (i < n) c(p_lab[i], as.character(round(i/n * 100)), '%'), sep = '')
  }

  res = character(n)
  for (i in seq_len(n)) {
    b = blocks[[i]]
    res[i] = xfun:::handle_error(
      if (b$type == 'code_chunk') {
        one_string(fuse_code(b, envir, blocks))
      } else {
        one_string(fuse_text(b, envir), '')
      },
      function(e, loc) sprintf('Quitting from lines %s', loc),
      p_lab[i], get_loc(b, input)
    )
    p_bar(i, n)
  }
  res
}

# extract code from input
fiss = function(input = NULL, output = NULL, text = xfun::read_utf8(input)) {
  fuse_wrapper(input, output, text, '', FALSE, '.R', .fiss)
}

.fiss = function(blocks) {
  res = lapply(blocks, function(b) {
    if (b$type == 'code_chunk' && !isFALSE(b$options$purl)) c(b$source, '')
  })
  unlist(res)
}

fuse_wrapper = function(input, output, text, format, template, ext, process_fun, ...) {
  if (auto_name <- is.null(output) && is_file(input)) {
    output = check_output(input, xfun::with_ext(input, ext))
  }

  # cleaning up some objects on exit
  opts = reactor(); on.exit(reactor(opts), add = TRUE)
  if (chunk_counter$get() == 0) on.exit(chunk_counter$reset(), add = TRUE)
  oenv = as.list(.env); on.exit(reset_env(oenv, .env), add = TRUE)

  # set working directory if unset
  if (is_file(input) && is.null(opts$wd)) opts$wd = dirname(normalizePath(input))

  # store output dir so we can calculate relative paths for plot files later
  .env$wd.out = xfun::normalize_path(
    if (is.character(output)) dirname(output) else {
      if (is.character(.env$wd.out)) .env$wd.out else '.'
    }
  )

  # set default device to 'pdf' for LaTeX output, and 'png' for other formats
  if (is.null(opts$dev)) {
    opts$dev = if (format == 'latex') 'pdf' else 'png'
  }
  # set fig.path to output_files/figure-format/ if `output` is a path, otherwise
  # use a temp path
  if (is.null(opts$fig.path)) {
    opts$fig.path = if (is.character(output)) {
      paste0(xfun::sans_ext(output), '_files/figure-', format, '/')
    } else tempfile('litedown-fuse-', '.')
  }
  # make sure fig.path is absolute path
  if (xfun::is_rel_path(opts$fig.path))
    opts$fig.path = file.path(getwd(), opts$fig.path)

  blocks = parse_rmd(input, text)
  res = process_fun(blocks, ...)

  if (format %in% c('html', 'latex')) {
    res = mark(text = res, format = format, template = template)
    if (auto_name) output = auto_output(input, format)
  }
  # TODO: build PDF for format == 'latex'?
  if (is.character(output)) {
    xfun::write_utf8(res, output)
    invisible(output)
  } else res
}

fuse_code = function(x, envir, blocks) {
  old = reactor(names(x$options)); on.exit(reactor(old), add = TRUE)
  # merge local chunk options into global options
  reactor(x$options)
  opts = reactor()

  # evaluate `wd` for now to make sure we set the right working directory before
  # evaluating anything else
  opts$wd = eval_lang(opts$wd, envir)
  if (is.character(opts$wd)) {
    owd = setwd(opts$wd); on.exit(setwd(owd), add = TRUE)
  }
  for (i in names(opts)) opts[[i]] = eval_lang(opts[[i]], envir)

  if (opts$engine != 'r') {
    warning("The engine '", opts$engine, "' is not supported yet.")
    return('')
  }

  # fuse child documents
  if (length(opts$child)) return(unlist(lapply(
    opts$child, fuse, output = NA, format = 'markdown', envir = envir, quiet = TRUE
  )))

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
    x$source = xfun::read_all(opts$file)
  } else if (test_source('code')) {
    x$source = opts$code
  } else if (test_source('ref.label')) {
    x$source = unlist(lapply(blocks[opts$ref.label], `[[`, 'source'))
  }

  args = reactor('fig.path', 'fig.ext', 'dev', 'dev.args', 'error')
  # map chunk options to record() argument names
  names(args)[1:2] = c('dev.path', 'dev.ext')
  args = dropNULL(args)
  lab = opts$label %||% sprintf('unnamed-chunk-%d', chunk_counter$inc())
  args$dev.path = paste0(args$dev.path, lab)
  args$dev.args = merge_list(
    list(width = opts$fig.width, height = opts$fig.height), opts$dev.args
  )

  res = if (opts$eval) {
    do.call(xfun::record, c(list(code = x$source, envir = envir), args))
  } else {
    list(structure(x$source, class = 'record_source'))
  }

  if (!opts$include) return('')

  # decide the number of backticks to wrap up output
  fence = xfun::make_fence(unlist(res))

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
  if (!is.null(env)) res = c(p1, structure(unlist(p2), class = 'record_plot'))
  i = 0  # plot counter

  # generate markdown output
  out = unlist(lapply(res, function(x) {
    type = sub('record_', '', class(x))
    if (type == 'source') {
      if (!opts$echo) return()
      x = one_string(x)
      if (opts$strip.white) x = str_trim(x)
    }
    if (type == 'output') {
      if (opts$results == 'hide') return()
      if (opts$results == 'asis') return(x)
    }
    if (type == 'warning' && !opts$warning) return()
    if (type == 'message' && !opts$message) return()
    if (type == 'plot') {
      n = length(x); i2 = i + seq_len(n); i <<- i + n
      img = sprintf(
        '![%s](<%s>)%s', alt[i2],
        if (is.null(fig.dir)) x else gsub('^.*/', fig.dir, x), att[i2]
      )
      if (is.null(env)) img else xfun::fenced_block(
        c(img, '', sprintf('<span>#fig:%s</span> %s', lab, cap)), env, char = ':'
      )
    } else {
      a = opts[[paste0('attr.', type)]]
      if (type == 'source') {
        a = c(paste0('.', opts$engine), a)  # use engine name as class name
      } else {
        x = paste0(opts$comment, x)  # comment out text output
      }
      xfun::fenced_block(x, a, fence)
    }
  }))
  if (!is.null(opts$attr.chunk)) out = xfun::fenced_block(out, opts$attr.chunk, char = ':')
  if (!is.null(x$prefix)) out = paste0(x$prefix, out)
  out
}

is_plot = function(x) inherits(x, 'record_plot')

fuse_text = function(x, envir) {
  if (is.character(src <- x$source)) return(src)
  res = lapply(src, function(s) {
    if (is.character(s)) s else exec_inline(s, envir)
  })
  unlist(res)
}

exec_inline = function(x, envir) {
  o = x$options
  if (o$engine != 'r') {
    warning("The inline engine '", o$engine, "' is not supported yet")
    return('')
  }
  res = eval(xfun::parse_only(x$code), envir)
  # TODO: allow for custom coercion functions here
  as.character(res)
}

# similar to the base R options() interface but for litedown chunk options
reactor = local({
  # global chunk options
  .opts = structure(new_env(), class = 'reactor_options')

  opt_get = function(x, drop = length(x) == 1) {
    vs = mget(x, .opts, ifnotfound = list(NULL))
    if (drop) vs[[1]] else vs
  }
  # setter: fun(opt = val); getter: fun('opt')
  opt_fun = function(...) {
    v = list(...)
    n = length(v)
    if (n == 0) return(.opts)
    if (is.null(nms <- names(v))) {
      if (all(vapply(v, is.character, TRUE))) return(opt_get(unlist(v)))
      if (is.null(nms <- names(v <- v[[1]])) || any(nms == '')) stop(
        'The first unnamed argument must take a value of a named list.'
      )
    }
    if (any(nms == '')) stop('All arguments must be either named or unnamed.')
    old = opt_get(nms, drop = FALSE)
    for (i in nms) assign(i, v[[i]], envir = .opts)
    invisible(old)
  }
  opt_fun(
    eval = TRUE, echo = TRUE, results = 'markup', comment = '#> ',
    warning = TRUE, message = TRUE, error = FALSE, include = TRUE,
    strip.white = TRUE,
    attr.source = NULL, attr.output = NULL, attr.plot = NULL, attr.chunk = NULL,
    attr.message = '.plain .message', attr.warning = '.plain .warning', attr.error = '.plain .error',
    cache = FALSE, cache.path = NULL,  # TODO: cache not implemented
    dev = NULL, dev.args = NULL, fig.path = NULL, fig.ext = NULL,
    fig.width = 7, fig.height = 7, fig.cap = NULL, fig.alt = NULL, fig.env = NULL,
    code = NULL, file = NULL, ref.label = NULL, child = NULL, purl = TRUE,
    wd = NULL
  )
  opt_fun
})

#' @exportS3Method
print.reactor_options = function(x, ...) {
  str(as.list.environment(x, all.names = TRUE, sorted = TRUE), ...)
  invisible(x)
}

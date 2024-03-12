mkd <- paste(names(litedown:::pants), collapse = ' ')
stopifnot(litedown:::smartypants(mkd) == '&frac12; &frac13; &frac23; &frac14; &frac34; &frac15; &frac25; &frac35; &frac45; &frac16; &frac56; &frac18; &frac38; &frac58; &frac78; &#8528; &#8529; &#8530; &copy; &reg; &trade;')

## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2015-06-17 16:42:05 CEST (es)>

## here <- function(s, drop = TRUE, guess.type = TRUE) {
##     ans <- readLines(textConnection(s))
##     if (drop && ans[len <- length(ans)] == "")
##         ans <- ans[-len]
##     if (drop && ans[1L] == "")
##         ans <- ans[-1L]
##     if (guess.type)
##         type.convert(ans, as.is = TRUE) else ans
## }

## -*- truncate-lines: t; -*-
## Time-stamp: <2014-08-27 14:39:02 CEST (es)>

nth <- function (x, n, first = 1L)
    if (is.matrix(x))
        x[seq(first, nrow(x), by = n), ] else
            x[seq(first, length(x), by = n)]

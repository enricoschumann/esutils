## -*- truncate-lines: t; -*-
## Time-stamp: <2014-07-31 09:44:19 CEST (es)>

nth <- function (x, n, first = 1L) 
    x[seq(first, length(x), by = n)]

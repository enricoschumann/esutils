## -*- truncate-lines: t; -*-
## Time-stamp: <2014-07-17 08:40:23 CEST (es)>

function (x, n, first = 1) 
    x[seq(first, length(x), by = n)]

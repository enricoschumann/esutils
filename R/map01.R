## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2015-06-17 16:42:49 CEST (es)>

## map01 <- function(x, old.min = min(x), old.max = max(x), new.min = 0, new.max = 1) {
##     if (old.min == old.max || new.min == new.max)
##         stop("check limits")
##     new.range <- new.max - new.min
##     old.range <- old.max - old.min
##     (new.range * x + new.min * old.max - new.max * old.min)/old.range
## }


## map01(as.numeric((index(dtw)[-1]- index(dtw)[1])), new.min=0.1, new.max=0.95)

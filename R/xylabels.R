## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2015-03-02 16:39:26 CET (es)>


xylabels <- function(x,y,labels, ...) {
    d <- par()$usr
    xx <- (d[2] - d[1]) * x + d[1]
    yy <- (d[4] - d[3]) * y + d[3]
    
    text(x = xx, y = yy, labels = labels, ...)
    c(xx,yy)
}

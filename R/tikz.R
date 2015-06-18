## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2015-06-18 06:20:23 CEST (es)>

if (FALSE) {
    set.seed(2891472)
    r <- rnorm(100, mean = 0.003, sd = 0.01)
    y <- (cumprod(1+r)-1)*25
    y <- y/max(y)*9
    y[1] <- 1
    x <- (1:100)/3

    r <- rnorm(100, mean = 0.003, sd = 0.01)
    y2 <- (cumprod(1+r)-1)*25
    y2 <- y2/max(y2)*6
    y2[1] <- 1

    tikz.points <- function(x, y, units = c("pt", "pt"), digits = 5) {
        paste(paste0("(", paste(round(x,digits), units[1]), ", ", 
                     paste(round(y,digits), units[2]), ")"), 
              collapse = " -- \n")
    }
setwd("/home/es/Packages/esutils/various")
cat("\\begin{tikzpicture}", file = "paths.tex")

cat("\\draw[line width=0.2pt,color=gray] \n", file = "paths.tex", append=TRUE)
cat(tikz.points(c(min(x),max(x)),c(0,0)),";\n", file = "paths.tex", append=TRUE)
cat("\\draw[line width=0.2pt,color=gray] \n", file = "paths.tex", append=TRUE)
cat(tikz.points(c(min(x),min(x)),c(0,9)),";\n", file = "paths.tex", append=TRUE)

cat("\\draw[line width=0.6pt,color=red] \n", file = "paths.tex", append=TRUE)
cat(tikz.points(x,y),";\n", file = "paths.tex", append=TRUE)

cat("\\draw[line width=0.6pt,color=blue] \n", file = "paths.tex", append=TRUE)
cat(tikz.points(x,y2),";\n", file = "paths.tex", append=TRUE)

cat("\\end{tikzpicture}", file = "paths.tex", append=TRUE)

.tikz.points <- function(x, y, units = c("pt", "pt"), digits = 5, path = "--") {
    if (length(units)==1L)
        units <- c(units, units)
    paste(paste0("(", paste(round(x, digits), units[1]), ", ", 
                      paste(round(y, digits), units[2]), ")"), 
          collapse = paste0(" ", path, " \n"))
}
.tikz.path <- function(x,y, units = c("pt", "pt"), digits = 5, path = "--",
                       width = "1 pt", color = "black") {

    paste0(paste("\\draw[line width = ", width, ", color = ", color,
                 "] \n"),
           .tikz.points(x, y, units, digits, path), ";")
}

cat(.tikz.path(c(3,4), c(1,6), units = "cm"), file = "tikz0.tex")
}

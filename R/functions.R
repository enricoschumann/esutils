## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

here <- function(s, drop = TRUE, guess.type = TRUE,
                 sep = NULL, header = TRUE, ...) {
    if (is.null(sep)) {
        ans <- readLines(textConnection(s))
        if (drop && ans[len <- length(ans)] == "")
            ans <- ans[-len]
        if (drop && ans[1L] == "")
            ans <- ans[-1L]
        if (guess.type)
            type.convert(ans, as.is = TRUE)
        else
            ans
    } else
        read.table(textConnection(s),
                   header = header, sep = sep, ...)    
}

ij_i <- function(i, j, nrow) {
    if (is.matrix(i))
        i[ ,1L] + (j[ ,2L] - 1) * nrow
    else
        i + (j - 1) *nrow
}

i_ij <- function(index, nrow) {
    x <- index %% nrow
    i <- ifelse(x == 0, nrow, x)
    j <- ifelse(x == 0, index%/%nrow, index%/%nrow + 1)
    cbind(i,j)
}

map01 <- function(x, min = 0, max = 1, omin = min(x), omax = max(x)) {
    new.range <- max - min
    old.range <- omax - omin
    (new.range * x + min * omax - max * omin)/old.range
}

nth <- function (x, n, first = 1L)
    if (is.matrix(x))
        x[seq(first, nrow(x), by = n), ] else x[seq(first, length(x), by = n)]

pdf2txt <- function(file, out, path.exec = "pdftotext", ..., layout = TRUE) {

    files <- file
    for (file in files) {
        basename <- unlist(strsplit(file, "[.][0-9a-zA-Z]*$"))

        if (missing(out))
            out <- paste0(basename, ".txt") else
        if (!is.na(fi <- file.info(out)$isdir) && fi)
            out <- file.path(out, paste0(basename, ".txt"))

        system(paste0(
            shQuote(path.exec),
            if (layout) " -layout " else "",
            shQuote(file), " ", shQuote(out)), ...)
    }
}

qrequire <- function(package, lib.loc = NULL, quietly = TRUE,
                     warn.conflicts = TRUE, character.only = FALSE,
                     file = tempfile()) {
    if (!character.only)
        package <- as.character(substitute(package))
    (suppressPackageStartupMessages(
        capture.output(ans <- require(package, lib.loc, quietly,
                                      warn.conflicts, character.only = TRUE),
                       file)))
    invisible(ans)
}

ss2csv <- function(file, out, path.exec = "ssconvert", ...) {

    files <- file
    for (file in files) {
        basename <- unlist(strsplit(file, "[.][0-9a-zA-Z]*$"))

        if (missing(out))
            out <- paste0(basename, "_%n.csv") else
        if (!is.na(fi <- file.info(out)$isdir) && fi)
            out <- file.path(out, paste0(basename, "_%n.csv"))

        system(paste0(
            shQuote(path.exec),
            " -T Gnumeric_stf:stf_csv -S ",
            shQuote(file), " ", shQuote(out)), ...)
    }
}

xy_text <- function(x,y,labels, ...) {
    d <- par()$usr
    xx <- (d[2] - d[1]) * as.numeric(x) + d[1]
    yy <- (d[4] - d[3]) * as.numeric(y) + d[3]

    text(x = xx, y = yy, labels = labels, ...)
    invisible(c(xx,yy))
}


## z <- array(runif(100),dim=c(5,3))
## i <- 3; j <- 2
## ij_i(3,2, nrow(z))

## # test 1: extract element with i,j and with index
## z[i,j]
## z[ij_i(i,j,dim(z)[1L])]

## # test 2: get i,j back from index
## index <- getIndex(i,j,dim(z)[1L])
## getIJ(index,dim(z)[1L])

## # vectorised
## index <- 1:7
## getIJ(index,dim(z)[1L])

## i <- rep(1,3); j <- 1:3
## z[cbind(i,j)]
## z[getIndex(i,j,dim(z)[1L])]


## path <- "~/Aquila"

## org
## [[file:sometextfile::NNN]]
## paste0("[[file:", path, "/",f,"::", ilines,"]]")

TODO <- function(path,
                 pattern = "[.]R$|[.]org$|[.]Rnw$",
                 recursive = TRUE,
                 lines.above = 0, lines.below = 0) {

    files <- list.files(path = path,
                        pattern = paste(c("[.]R$", "[.]org$", "[.]Rnw$"),
                        collapse = "|"),
                        recursive = TRUE,
                        full.names = TRUE)

    res <- character(0L)
    offset <- (-lines.above):lines.below
    for (f in files) {
        lines <- readLines(f, warn = FALSE)
        if (any(ilines <- grep("TODO", lines, useBytes = TRUE))) {
            ## browser()
            cat("\n=== ", f, " : ", 
                paste(ilines, collapse = ", "), "\n", sep = "",
                paste(lines[rep(ilines, each = length(offset))  + offset],
                      collapse = "\n"),
                "\n===\n")
            cat("\n\n")
            
        }
    }
    invisible(NULL)
}

matrixImage <- function(X, row.labels, col.labels, cex.axis = 1, grid = FALSE) {
    ## TODO add grid
    par(las = 2)
    m <- abs(t(X[nrow(X):1, ]))
    mr <- nrow(m)
    mc <- ncol(m)
    if (missing(row.labels)) {
        row.labels <- if (is.null(rownames(X)))
                          rev(seq_len(mc)) else rownames(X)
    }
    if (missing(col.labels)) {
        col.labels <- if (is.null(colnames(X)))
                          rev(seq_len(mr)) else colnames(X)
    }

    rsq <- seq_along(row.labels)
    irow <- rsq[match(pretty(rsq, n = 10), rsq, nomatch = 0)]
    csq <- seq_along(col.labels)
    icol <- csq[match(pretty(csq, n = 10), csq, nomatch = 0)]

    image(1:mr, 1:mc, m, col = c("white", "blue"),
          xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(1, at = seq_len(mr)[icol], labels = col.labels[icol],
         lty = 0, cex.axis = cex.axis)
    axis(2, at = seq_len(mc)[irow], labels = rev(row.labels[irow]),
         lty = 0, cex.axis = cex.axis)
    invisible()
}

## xxx <- array(0, dim = c(4,2))
## xxx[1:2,2] <- 1
## rownames(xxx) <- row.labels <- letters[1:nrow(xxx)]
## colnames(xxx) <- col.labels <- LETTERS[1:ncol(xxx)]
## matrixImage(xxx)

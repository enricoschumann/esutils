## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

here <- function(s, drop = TRUE, guess.type = TRUE,
                 sep = NULL, header = TRUE,
                 stringsAsFactors = FALSE,
                 trim = TRUE, ...) {
    ans <- readLines(textConnection(s))
    
    if (drop && ans[len <- length(ans)] == "")
        ans <- ans[-len]
    if (drop && ans[1L] == "")
        ans <- ans[-1L]

    if (is.null(sep) && guess.type)
        ans <- type.convert(ans, as.is = TRUE)
    else {
        ans <- read.table(text = ans,
                          header = header, sep = sep,
                          stringsAsFactors = stringsAsFactors,
                          strip.white = trim,
                          colClasses = if (guess.type)
                                NA else "character", ...)
    }
    ans
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

map01 <- function(x, min = 0, max = 1, omin = min(x), omax = max(x), na.rm = FALSE) {
    if (na.rm)
        message("not yet supported")
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


## path <- "~/Documents"

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

matrix_image <- function(X, row.labels, col.labels, cex.axis = 1, grid = FALSE) {
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
## matrix_image(xxx)

package.skeleton2 <- function(name = "anRpackage",
                              list = character(),
                              environment = .GlobalEnv,
                              path = ".",
                              force = FALSE,
                              code_files = character()) {


    ## .gitignore
    ## .keywords
    ## ChangeLog
    ## NEWS
    ## README.org
    ## THANKS
    ## TODO_<pkg>
    ## .Rbuildignore

##    package.skeleton
    
}

fun_names <- function(dir,
                      duplicates_only = TRUE,
                      file_pattern = "[.][rR]$",
                      fun_pattern = " *([^\\s]+) *<- *function.*") {

    files <- dir(dir, pattern = file_pattern, full.names = TRUE)
    ans <- data.frame(fun = character(0),
                      file = character(0))
    for (f in files) {
        txt <- readLines(f)
        fun.lines <- grepl(fun_pattern, txt)
        
        if (any(fun.lines)) {
            ans <- rbind(ans,
                         data.frame(fun = gsub(fun_pattern, "\\1",
                                               txt[fun.lines],
                                               perl = TRUE),
                                    file = f,
                                    line = which(fun.lines),
                                    stringsAsFactors = FALSE))
        }
    }

    ans <- ans[order(ans[["fun"]]), ]

    if (duplicates_only) {
        d <- duplicated(ans[["fun"]])
        d0 <- match(unique(ans[["fun"]][d]), ans[["fun"]])
        ans <- ans[sort(c(d0, which(d))),]
    }

    ans
}

## takes a string like "12.000,23" and returns 12000.23
char2num <- function(s, dec = ",", big.mark = ".") {
    s <- gsub(big.mark, "", s, fixed = TRUE)
    as.numeric(sub(dec, Sys.localeconv()[["decimal_point"]], s, fixed = TRUE))
}

header <- function(h, width = 55,
                   line = "-",
                   open = " [[ ",
                   close = " ]] ",
                   line.start = "## ",
                   line.end = "") {

    nc <- nchar(h)
    left <- width - (nc + nchar(line.start) + nchar(line.end) +
                     nchar(open) + nchar(close))
    lines <- character(length(h))
    for (i in 1:length(h)) {
        lines[i] <- paste0(rep(line, trunc(left[[i]]/2)), collapse = "")
    }
    paste0(line.start,
           ifelse(left %% 2, " ", ""),
           lines, open, h, close, lines,
           line.end)
}

latest_version <- function(pkg, path = ".") {
    all_p <- dir(path, pattern = paste0(pkg, ".*tar[.]gz"))
    all_v <- gsub(".*_([0-9]+[.][0-9]+-[0-9]+)[.]tar[.]gz", "\\1", all_p)
    all_v  <- package_version(all_v)
    all_p[max(all_v) == all_v]
}

make_tex <- function(fn, sweave = TRUE, weaver = FALSE,
                     encoding = "utf8", latexmk = FALSE) {
    ## encoding "" is default for Sweave
    if (sweave)
        if (weaver && requireNamespace("weaver"))
            Sweave(fn, driver = weaver::weaver(), encoding = "utf8")
        else
            Sweave(fn, encoding = "utf8")
    if (latexmk)
        system(paste("latexmk -lualatex", gsub("Rnw$", "tex", fn)))
}

pkg_build <- function(pkg, parent.dir = ".",
                      check = FALSE,
                      build.vignettes = TRUE,
                      run.tests = TRUE,
                      install = FALSE,
                      clean = FALSE,
                      bump.version = FALSE,
                      resave.data = TRUE) {
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(parent.dir)

    if (bump.version) {
        ## TODO allow major/minor/patch
        D_file <- file.path(pkg, "DESCRIPTION")
        D <- readLines(D_file)
        i <- grep("^Version: ", D)
        v1 <- as.numeric(gsub(".*-(.*)", "\\1", D[i])) + 1
        D[i] <- gsub("(.*-).*", paste0("\\1", v1), D[i])
        i <- grep("^Date: ", D)
        D[i] <- paste("Date:", Sys.Date())
        writeLines(D, D_file)
    }
    if (build.vignettes)
        system(paste("R CMD build --resave-data=best", pkg))
    else
        system(paste("R CMD build --resave-data=best --no-build-vignettes", pkg))

    if (run.tests) {
        Sys.setenv("ES_PACKAGE_TESTING"=TRUE)
        ans <- try(source(file.path(pkg, "inst", "unitTests", "runTests.R")), silent = TRUE)
        Sys.setenv("ES_PACKAGE_TESTING"=FALSE)
        if (inherits(ans, "try-error"))
            message("check is TRUE but no unit tests found")
        else
            try(browseURL(file.path(getwd(), pkg, "inst", "unitTests", "test_results.txt")), silent = TRUE)
    }

    if (install)
        system(paste0("R CMD INSTALL --merge-multiarch ", esutils::latest_version(pkg)))

    if (check) {
        Sys.setenv("ES_PACKAGE_TESTING"=TRUE)
        system(paste0("R CMD check ", esutils::latest_version(pkg)))
        Sys.setenv("ES_PACKAGE_TESTING"=FALSE)
        ## browseURL(file.path(getwd(), paste0(pkg, ".Rcheck"),
        ##                     "inst", "unitTests", "test_results.txt"))
    }
    
    if (clean) {
        unlink(paste0(pkg, ".Rcheck"), TRUE, TRUE)
        unlink(dir(pattern = paste0("^", pkg, ".*[.]tar[.]gz$")))
    }

    invisible(NULL)
}

pkg_clean <- function(do = FALSE,
                      pkg = ".*" ,
                      parent.dir = ".",
                      keep.latest = FALSE) {
    
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(parent.dir)
    ans <- 0
    d <- dir(pattern = paste0(pkg, ".Rcheck"))
    if (!length(d)) 
        cat("No Rcheck directories found.\n")
    else {
        cat("Rcheck directories found:\n")
        cat(sort(paste(" ", d)), sep = "\n")
    }
    if (length(d) && do) {
        ans <- unlink(d, TRUE, TRUE)
        cat("\n  ... removed.\n\n")
    }

    d <- dir(pattern = paste0("^", pkg, ".*[.]tar[.]gz$"))
    if (!length(d)) 
        cat("No tarballs found.\n")
    else {
        cat("Tarballs found:\n")
        cat(sort(paste(" ", d)), sep = "\n")
    }
    if (length(d) && do) {
        ans <- unlink(d, TRUE, TRUE)
        cat("\n  ... removed.\n")
    }

    
    invisible(ans)
}

short_fn <- function(x, length = 50) {
    
    if (!length(x))
        return(character(0))
    
    bname <- gsub("(.*)[.][^.]*", "\\1", x)
    ext <- gsub(".*[.]([^.]*)", "\\1", x)
    
    ## make underscore
    chars <- c("[", "]", ".",
               ",", ";", "+",
               "%20", "%2E",
               "(", ")", "&", " ")
    
    for (ch in chars)
        bname <- gsub(ch, "_", bname, fixed = TRUE)
    
    ## replace single characters
    ## bname <- gsub("[^[:alpha:]][[:alpha:]][^[:alpha:]]", "_", bname)
    
    ## replace multiple _ with a single _
    chars <- c("__*")
    for (ch in chars)
        bname <- gsub(ch, "_", bname)
    
    ## remove patters/chars
    chars <- c("^_", "'", "‘", "’")
    for (ch in chars)
        bname <- gsub(ch, "", bname)
    
    ## replace phrases
    phrases <- c("value-at-risk", "VaR",
                 "volatility", "vol",
                 "ä", "ae",
                 "ö", "oe",
                 "ü", "ue",
                 "ß", "ss")
    for (i in seq(1, length(phrases), by = 2))
        bname <- gsub(phrases[i], phrases[i+1], bname, ignore.case = TRUE)
    
    bname <- substr(bname, 1, length)
    
    ## remove trailing _
    chars <- c("_$")
    for (ch in chars)
        bname <- gsub(ch, "\\1", bname)
    
    paste0(bname, ".", ext)
}


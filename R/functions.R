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


map01 <- function(x, min = 0, max = 1, omin = min(x), omax = max(x), na.rm = FALSE) {
    if (na.rm)
        message("not yet supported")
    new.range <- max - min
    old.range <- omax - omin
    (new.range * x + min * omax - max * omin)/old.range
}

nth <- function (x, n, first = 1L) {
    i <- seq(first, NROW(x), by = n)
    if (is.matrix(x))
        x[i, ]
    else
        x[i]
}
    
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

latest_version <- function(pkg, path = ".", type = "source") {
    if (type == "source")
        ext <- "[.]tar[.]gz"
    else if (type == "binary" || type == "zip")
        ext <- "[.]zip"
    all_p <- dir(path, pattern = paste0(pkg, ".*", ext))
    all_v <- gsub(paste0(".*_([0-9]+[.][0-9]+-[0-9]+)", ext),
                  "\\1", all_p)
    all_v  <- package_version(all_v)
    all_p[max(all_v) == all_v]
}

make_tex <- function(fn, sweave = TRUE, weaver = FALSE,
                     encoding = "utf8", latexmk = FALSE) {
    ## encoding "" is default for Sweave
    if (sweave)
        if (weaver && require("weaver"))
            Sweave(fn, encoding = "utf8",
                   driver = weaver::weaver())
        else
            Sweave(fn, encoding = "utf8")
    if (latexmk)
        system(paste("latexmk -lualatex",
                     gsub("Rnw$", "tex", fn)))
}

pkg_build <- function(pkg, parent.dir = ".",
                      check = FALSE,
                      build.vignettes = TRUE,
                      run.tests = TRUE,
                      install = FALSE,
                      keep.source = FALSE,
                      clean = FALSE,
                      bump.version = FALSE,
                      bump.date = FALSE,
                      resave.data = TRUE,
                      show.test.results = TRUE,
                      verbose = TRUE) {

    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(parent.dir)

    old.crayon <- getOption("crayon.enabled")
    on.exit(options(crayon.enabled = old.crayon))
    options(crayon.enabled = TRUE)

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

    if (bump.date) {
        ## TODO allow major/minor/patch
        D_file <- file.path(pkg, "DESCRIPTION")
        D <- readLines(D_file)
        i <- grep("^Date: ", D)
        D[i] <- paste("Date:", Sys.Date())
        writeLines(D, D_file)
    }
    
    ## R CMD build
    if (verbose)
        message("Building package ", pkg, " ... ",
                appendLF = FALSE)
    msg <- Rcmd(c("build", 
                  if (resave.data)      "--resave-data=best",
                  if (!build.vignettes) "--no-build-vignettes",
                  pkg),
                stdout = TRUE, stderr = TRUE)
    error <- any(grepl("ERROR", msg, ignore.case = TRUE))
    if (verbose && !error)
        message(green("[OK]"))
    else if (verbose && error)
        message(red("[ERROR]"))


    ## Unit tests
    ## ... assumes a directory inst/unitTests with a file
    ##     runTests.R
    if (run.tests) {
        if (verbose)
            message("Running tests ... ", appendLF = FALSE)
        Sys.setenv("ES_PACKAGE_TESTING" = TRUE)
        ans <- try(source(file.path(pkg,
                                    "inst",
                                    "unitTests",
                                    "runTests.R")),
                   silent = TRUE)
        Sys.setenv("ES_PACKAGE_TESTING" = FALSE)
        if (inherits(ans, "try-error")) {
            message(sQuote("run.tests"),
                    " is TRUE but no unit tests found ... ",
                    appendLF = FALSE)
            test.res <- "0 errors, 0 failures"
        } else {
            test.res <- readLines(
                file.path(pkg, "inst", "unitTests",
                          "test_results.txt"))
            test.res <- test.res[grep(" - [0-9]+ test functions", test.res)]
            test.res <- gsub(".*test functions, ", "", test.res)
        }
        if (!inherits(ans, "try-error") && show.test.results)
            try(browseURL(
                normalizePath(
                    file.path(pkg, "inst", "unitTests",
                              "test_results.txt"))),
                silent = TRUE)

        
        error <- if (test.res != "0 errors, 0 failures")
                     TRUE else FALSE
        
        if (verbose && error)
            message(red(paste0("[", test.res, "]")))
        else if (verbose)
            message(green("[OK]"))
    }

    
    ## R CMD INSTALL
    if (install) {
        if (verbose)
            message("Installing ... ", appendLF = FALSE)
        msg1 <- c(msg,
                  Rcmd(c("INSTALL",
                         "--merge-multiarch",
                         "--byte-compile",
                         if (keep.source) "--with-keep.source",
                         latest_version(pkg)),
                       stdout = TRUE, stderr = TRUE))
        
        error <- any(grepl("ERROR", msg1, ignore.case = TRUE))
        msg <- c(msg, msg1)
        if (verbose && !error)
            message(green("[OK]"))
        else if (verbose && error)
            message(red("[ERROR]"))
    }

    
    ## R CMD check
    if (check) {
        if (verbose)
            message("Running R CMD check ... ", appendLF = FALSE)

        msg1 <- Rcmd(c("check", latest_version(pkg)),
                     stdout = TRUE, stderr = TRUE)
        
        msg <- c(msg, msg1)
        check.res <- gsub("Status: (.*)", "\\1", msg1[grep("^Status: ", msg1)])
        error.warn <- grepl("error|warning", check.res, ignore.case = TRUE)
        note <- grepl("note", check.res, ignore.case = TRUE)
        if (verbose && !error.warn && !note)
            message(green("[OK]"))
        else if (verbose && error.warn)
            message(red(paste0("[", check.res ,"]")))
        else if (verbose && note)
            message(yellow(paste0("[", check.res ,"]")))
        else if (verbose)
            message(green("[OK]"))
    }
    
    if (clean) {
        if (verbose)
            message("Removing check files ... ", appendLF = FALSE)
        unlink(paste0(pkg, ".Rcheck"), TRUE, TRUE)
        unlink(dir(pattern = paste0("^", pkg, ".*[.]tar[.]gz$")))
        if (verbose)
            message(green("[OK]"))
    }

    invisible(msg)
}

pkg_clean <- function(do = FALSE,
                      pkg = ".*" ,
                      parent.dir = ".",
                      keep.latest = FALSE,
                      silent = FALSE) {
    
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(parent.dir)
    ans <- 0
    d <- dir(pattern = paste0(pkg, ".Rcheck"))
    if (!silent) {
        if (!length(d)) 
            cat("No Rcheck directories found.\n")
        else {
            cat("Rcheck directories found:\n")
            cat(sort(paste(" ", d)), sep = "\n")
        }
    }
    if (length(d) && do) {
        ans <- unlink(d, TRUE, TRUE)
        if (!silent)
            cat("\n  ... removed.\n\n")
    }

    d <- dir(pattern = paste0("^", pkg, ".*[.]tar[.]gz$"))
    if (!silent) {
        if (!length(d)) 
            cat("No tarballs found.\n")
        else {
            cat("Tarballs found:\n")
            cat(sort(paste(" ", d)), sep = "\n")
        }
    }
    if (length(d) && do) {
        ans <- unlink(d, TRUE, TRUE)
        if (!silent)
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

search_files <- function(search, path, file.pattern = NULL, recursive = TRUE, ...) {
    files <- dir(path, pattern = file.pattern, recursive = recursive, ...)
    on.exit(getwd())
    setwd(path)
    for (f in files) {
        src <- readLines(f, warn = FALSE)
        lines <- grep(search, src)
        if (length(lines)) {
            cat(paste0(f, "::", lines, "::", src[lines]), sep = "\n")
        }
    }
    invisible(NULL)
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

add_toc <- function(txt, number = FALSE,
                    re.header = "^## *--+ *\\[(.+)\\] *--+ *",
                    re.toc.begin = "^## *contents",
                    re.toc.end = "^## /",
                    ignore.case = TRUE,
                    elisp = TRUE) {
    txt <- readLines("~/Desktop/topics.txt")
    hlines <- grepl(re.header, txt)
    htexts <- trim(gsub(re.header, "\\1", txt[hlines]))
    txt[hlines] <- header(htexts)
    
}


eol <- "\r\n"
fold <- "\r\n "

allday_event <- function(date, summary, description = "", file) {

    UID <- paste0(mailtools:::msgID(), ".", Sys.info()["nodename"])
    DTSTAMP <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y%m%dT%H%M%SZ")

    DTSTART <- format(date, "%Y%m%d")

    
tmp <-     
"BEGIN:VCALENDAR
VERSION:2.0
PRODID: esutils
BEGIN:VEVENT
UID:{UID}
DTSTAMP:{DTSTAMP}
DTSTART;VALUE=DATE:{DTSTART}
SUMMARY:{SUMMARY}
DESCRIPTION:{DESCRIPTION}
END:VEVENT
END:VCALENDAR"

    tmp <- fill_in(tmp,
                   UID = UID,
                   DTSTAMP = DTSTAMP,
                   DTSTART = DTSTART,
                   SUMMARY = summary,
                   DESCRIPTION = description)
    tmp <- strsplit(tmp, "\n", fixed = TRUE)[[1L]]
    tmp <- paste(tmp, collapse = eol)
    if (!missing(file)) {
        writeLines(tmp, file)
        invisible(tmp)
    } else
        tmp
    
}

bib_temp_key <- function(file, encoding = "UTF-8") {

    txt <- readLines(file, encoding = encoding)

    temp_keys <- grep("@[a-zA-Z]+\\{TODO[0-9]+,\\s*$", txt)
    n <- max(as.numeric(gsub("@[a-zA-Z]+\\{TODO([0-9]+),\\s*$", "\\1", txt[temp_keys])))

    missing_keys <- grep("@[a-zA-Z]+\\{,\\s*$", txt)
    txt[missing_keys] <- 
        paste0(gsub("(.*),\\s*$", "\\1", txt[missing_keys]),
               "TODO",
               seq(n + 1, by = 1, length.out = length(missing_keys)),
               ",")
    writeLines(txt, file)
}

wait <- function(x) 
    if (length(x) == 1L)
        Sys.sleep(x) else Sys.sleep(runif(1L, min(x), max(x)))


insert <- function(x, list, values) {
    len <- length(list) * (length(values) - 1L) + length(x)
    ans <- vector(typeof(x), length = len)
    seq_len(len)
}


## -*- truncate-lines: t; -*-
## Time-stamp: <2015-06-17 16:43:14 CEST (es)>

## pdf2txt <- function(file, out, path.exec = "pdftotext", ..., layout = TRUE) {

##     files <- file    
##     for (file in files) {
##         basename <- unlist(strsplit(file, "[.][0-9a-zA-Z]*$"))
        
##         if (missing(out))
##             out <- paste0(basename, ".txt") else
##         if (!is.na(fi <- file.info(out)$isdir) && fi)
##             out <- file.path(out, paste0(basename, ".txt"))
        
##         system(paste0(
##             shQuote(path.exec),
##             if (layout) " -layout " else "",
##             shQuote(file), " ", shQuote(out)), ...)
##     }
## }

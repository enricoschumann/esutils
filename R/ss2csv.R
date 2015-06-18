## -*- truncate-lines: t; -*-
## Time-stamp: <2015-06-17 16:43:49 CEST (es)>

## ss2csv <- function(file, out, path.exec = "ssconvert", ...) {

##     files <- file    
##     for (file in files) {
##         basename <- unlist(strsplit(file, "[.][0-9a-zA-Z]*$"))
        
##         if (missing(out))
##             out <- paste0(basename, "_%n.csv") else
##         if (!is.na(fi <- file.info(out)$isdir) && fi)
##             out <- file.path(out, paste0(basename, "_%n.csv"))
        
##         system(paste0(
##             shQuote(path.exec),
##             " -T Gnumeric_stf:stf_csv -S ",
##             shQuote(file), " ", shQuote(out)), ...)
##     }
## }

## -*- truncate-lines: t; -*-
## Time-stamp: <2014-07-31 08:52:27 CEST (es)>

ss2csv <- function(file, out, path.exec = "ssconvert", ...) {

    files <- file    
    for (file in files) {
        basename <- unlist(strsplit(file, "[.][0-9a-zA-Z]*$"))
        
        if (missing(out))
            out <- paste0(basename, "%n.csv") else
        if (!is.na(fi <- file.info(out)$isdir) && fi)
            out <- file.path(out, paste0(basename, "%n.csv"))
        
        system(paste0(
            shQuote(path.exec),
            " -T Gnumeric_stf:stf_csv -S ",
            file, " ", out), ...)
    }
}

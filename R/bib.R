number_todo <- function(file) {

    ## file <- "~/Documents/Library/Library.bib"
    txt <- readLines(file)

    ## count TODOs
    ii <- grep("@[a-zA-Z]+\\{TODO[0-9]*,", txt)
    nmax <- max(as.numeric(gsub(".*TODO(.*),", "\\1", txt[ii])))

    ii <- grep("@[a-zA-Z]+\\{\\s*,", txt)
    repl <- paste0(gsub(",\\s*$", "", txt[ii]),
                   "TODO", seq(from = nmax+1, length.out = length(ii)),
                   ",")

    txt[ii] <- repl
    writeLines(txt, con = file)
}

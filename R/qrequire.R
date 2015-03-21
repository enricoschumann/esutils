## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-
## Time-stamp: <2014-11-07 06:26:57 CET (es)>

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


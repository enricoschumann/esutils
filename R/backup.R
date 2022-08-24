create_backup <- function(dir.to.backup,
                          backup.filename.dir,
                          backup.filename = NULL,
                          exclude = NULL,
                          ignore.case = FALSE) {

    backup.filename <-
        paste0(format(Sys.time(), "%Y%m%d_%H%M%S"),
               if (!is.null(backup.filename)) "__",
               backup.filename, ".zip")

    zip.file <- path.expand(file.path(backup.filename.dir,
                                      backup.filename))

    files <- path.expand(
        list.files(dir.to.backup,
                   all.files = TRUE,
                   include.dirs = FALSE,
                   recursive = TRUE))
    missing.files <- !file.exists(file.path(dir.to.backup,files))
    if (any(missing.files)) {
        warning("some files do not exist")
        files <- files[!missing.files]
    }

    if (length(exclude)) {
        for (pattern in exclude) {
            x <- grepl(pattern, files, ignore.case = ignore.case)
            files <- files[!x]
        }
    }

    ans <- zip::zip(zipfile = zip.file,
                    files = files,
                    recurse = TRUE,
                    compression_level = 9,
                    mode = "mirror",
                    root = path.expand(dir.to.backup))
    invisible(ans)
}

extract_backup <- function(backup.filename,
                           files = NULL,
                           exdir = ".",
                           overwrite = TRUE) {
    ans <- zip::unzip(backup.filename,
                      exdir = path.expand(exdir),
                      files = files,
                      overwrite = TRUE)
    invisible(ans)
}

backup_filename <- function(root, UTC = FALSE, ...) {
    time <- format(Sys.time(), "%Y-%m-%d_%H%M%S",
           tz = if (UTC) "UTC" else "")
    node <- Sys.info()["nodename"]
    paste0(time, "__", node, "___", root)
}

flatten <- function(dir, out.dir, pattern = NULL) {
    files <- list.files(dir, full.names = TRUE, recursive = TRUE,
                        pattern = pattern)
    new.files <- make.unique(basename(files), sep = "_")
    copied <- file.copy(files,
                        file.path(out.dir, new.files))
    if (any(!copied))
        warning("could not copy ", files[!copied])
    invisible(sum(copied))
}

path8.3 <- function(path = ".") {
    if (.Platform$OS.type != "windows")
        returns(path)
    old.options <- options(useFancyQuotes = FALSE)
    on.exit(options(old.options))
    p <- paste(shQuote(path), collapse = " ")
    txt <- system2("cmd",
                   c("/C",
                     paste0(" for %I in (", p, ") do echo %~fsI")),
            stdout = TRUE, stderr = TRUE, invisible = TRUE)
    ans <- txt[seq(3, length(txt), by = 3)]
    names(ans) <- path
    ans
}

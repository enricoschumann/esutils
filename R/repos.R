find_git <- function(path = ".", tilde = TRUE, max.char = NA,...) {

    f <- dir(path = path,
             include.dirs = TRUE,
             recursive = TRUE, full.names = TRUE,
             all.files = TRUE)
    if (is.finite(max.char))
        f <- f[nchar(f) <= max.char]
    f <- f[basename(f) == ".git"]
    f <- f[file.info(f, extra_cols = FALSE)$isdir]
    f <- normalizePath(f)
    f <- sort(unique(dirname(f)))
    if (tilde)
        f <- sub(normalizePath(path.expand("~"), winslash = "/"),
                 "~", f, fixed = TRUE)
    class(f) <- c("git_list", class(f))
    f
}

fetch_git_info <- function(path, ...) {

    paths <- path
    clean <- rep(NA, length(paths))
    remotes <- rep(NA_character_, length(paths))
    remote_urls <- rep(NA_character_, length(paths))
    branches <- rep(NA_character_, length(paths))

    for (path in paths) {
        p <- path == paths
        br <- sort(names(git2r::branches(path, "local")))

        branches[p] <- paste(br, collapse = ";")



        st <- git2r::status(path)
        clean[p] <- !(length(st$staged) || length(st$unstaged) || length(st$untracked))

        rem <- git2r::remotes(path)
        remotes[p] <- paste(rem, collapse = ";")
        remote_urls[p] <- paste(git2r::remote_url(path, remote = rem), collapse = ";")
    }
    data.frame(path = paths,
               machine = Sys.info()[["nodename"]],
               ## user = Sys.info()["user"],
               clean = clean,
               remotes = remotes,
               remote_urls = remote_urls,
               branches = branches,
               check.names = FALSE,
               stringsAsFactors = FALSE)
}

git_bundle_create <- function(repos, output.filenames,
                              output.dir,
                              dated.bundle = TRUE,
                              overwrite = TRUE) {
    if (!dir.exists(output.dir)) {
        ans <- askYesNo("Create directory?")
        if (is.na(ans) || !ans)
            return(invisible(NULL))
        else
            dir.create(path.expand(output.dir))
    }
    current.dir <- getwd()
    on.exit(setwd(current.dir))
    for (i in seq_along(repos)) {
        message(repos[i], "\n",
                " =>")
        if (!dir.exists(repos[i]))
            message("    repository does not exist => skip")
        else
            message("    ", output.filenames[i], "\n")

        setwd(repos[i])
        bundle <- paste0(
            strftime(Sys.time(), "%Y%m%d_%H%M%S__"),
            "temp.bundle")
        system2("git",
                c("bundle", "create", bundle,
                  "--branches", "--tags"))
        message("")
        if (endsWith(output.filenames[i], ".bundle")) out.file <- output.filenames[i] else
        out.file <- paste0(output.filenames[i], ".bundle")
        file.copy(bundle, file.path(output.dir, out.file),
                  overwrite = overwrite)

        if (dated.bundle) {
            out.file <- paste0(
                strftime(Sys.time(), "%Y%m%d_%H%M%S__"),
                out.file)
            file.copy(bundle, file.path(output.dir, out.file),
                      overwrite = overwrite)
        }

        ignore <- file.remove(bundle)
    }
    invisible(NULL)
}

git_bundle_pull <- function(bundle, target, branch = "master") {

    if (!dir.exists(target))
        stop("'target' does not exist. Maybe clone?")

    current.dir <- getwd()
    on.exit(setwd(current.dir))

    setwd(target)
    system2("git", c("pull", bundle, branch))
}

git_bundle_clone <- function(bundle, dir.name, parent.dir) {

    if (dir.exists(file.path(parent.dir, dir.name)))
        stop("'dir.name' already exists. Maybe pull?")

    current.dir <- getwd()
    on.exit(setwd(current.dir))

    setwd(parent.dir)
    system2("git", c("clone", "-b", "master", bundle, dir.name))
}


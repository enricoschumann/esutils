find_git <- function(path = ".", tilde = TRUE, ...) {

    f <- dir(path = path,
             include.dirs = TRUE,
             recursive = TRUE, full.names = TRUE,
             all.files = TRUE)
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

fetch_git_info <- function(paths, ...) {

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

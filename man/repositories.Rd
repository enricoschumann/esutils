\name{repositories}
\alias{find_git}
\alias{fetch_git_info}
\title{
  Helper Functions for Managing Local Git Repositories
}
\description{
  Helper functions for managing local git repositories.
}
\usage{
find_git(path = ".", tilde = TRUE, ...)
fetch_git_info(paths, ...)
}
\arguments{
  \item{path}{
    character
  }
}
\details{

  To be added
  
}
\value{
  character vector
  data.frame
}
\author{
  Enrico Schumann
}
\seealso{
  the git2r package
}
\examples{
\donttest{
path <- "~/Packages"
repos <- find_git(path)
fetch_git_info(repos)[, c("path", "clean", "remote_urls")]
}
}

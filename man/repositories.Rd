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
find_git(path = ".", tilde = TRUE, max.char, ...)
fetch_git_info(path, ...)
}
\arguments{
  \item{path}{
    character
  }
  \item{tilde}{
    logical: do tilde expansion through \code{\link{path.expand}}?
  }
  \item{max.char}{\code{\link{NA}} or integer: if the latter,
    filepaths longer than
    \code{max.char} are excluded. (Necessary for operating
    systems that impose maximum path lengths.)

  }
  \item{\dots}{
    other arguments
  }
}
\details{

  Find git repositories, i.e. directories with name \code{.git}.

}
\value{
  a character vector for \code{find_git},
  a \code{\link{data.frame}} for \code{fetch_git_info}
}
\author{
  Enrico Schumann
}
\seealso{
  the \pkg{git2r} package
}
\examples{
\donttest{
path <- "~/Packages"
repos <- find_git(path)
fetch_git_info(repos)[, c("path", "clean", "remote_urls")]
}
}

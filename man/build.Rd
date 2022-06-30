\name{pkg_build}
\alias{pkg_build}
\alias{pkg_clean}
\alias{build_pkg}
\alias{latest_version}
\title{
    Tools for Building and Installing R Packages
}
\description{
    Building, checking and installing R packages.
}
\usage{
    pkg_build(pkg, parent.dir = ".", check = FALSE,
              build.vignettes = TRUE,
              run.tests = TRUE, install = FALSE,
              keep.source = FALSE,
              clean = FALSE,
              bump.version = FALSE,
              bump.date = FALSE,
              resave.data = TRUE,
              as.cran = FALSE,
              show.test.results = TRUE,
              verbose = TRUE,
              use.crayon = FALSE)
    latest_version(pkg, path = ".", type = "source")
}
\arguments{
  \item{pkg}{
    package name
  }
  \item{parent.dir}{
    a string: the path in which the source directories reside
  }
  \item{type}{
    string: \code{source} or \code{zip}
  }
  \item{as.cran}{
    logical
  }
}
\details{

  \code{pkg_build} builds a package from a source directory, and
  optionally checks and installs it.

}
\value{
  a character vector, insisibly
}
\author{
  Enrico Schumann
}
%% \seealso{
%%   \code{\link{Extract}}
%% }
\examples{
\donttest{
basedir <- "~/Packages"
packages <- c("NMOF", "PMwR")

for (p in packages)
    pkg_build(pkg = p,
              parent.dir = basedir,
              install = TRUE,
              check = TRUE,
              clean = FALSE,
              keep.source = TRUE)
}
}

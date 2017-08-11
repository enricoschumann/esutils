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
              clean = FALSE, bump.version = FALSE) 
    latest_version(pkg, path = ".") 
}
\arguments{
  \item{x}{
    a vector (or a list)
}
  \item{n}{
    an integer
}
  \item{first}{
    where to start
}
}
\details{

    \code{pkg_build}
}
\value{
  a vector (or a list)
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{Extract}}
}
\examples{
nth(1:20, 2, 1)
nth(1:20, 2, 2)
}

\name{qrequire}
\alias{qrequire}
\title{
  Loading/Attaching of Packages, but quietly
}
\description{
  Load package quietly.
}
\usage{
qrequire(package, lib.loc = NULL, quietly = TRUE,
         warn.conflicts = TRUE, character.only = FALSE,
         file = tempfile())
}
\arguments{
  \item{package}{
    see \code{\link{require}}
}
  \item{lib.loc}{
    \code{\link{require}}
}
  \item{quietly}{
\code{\link{require}}
}
  \item{warn.conflicts}{
\code{\link{require}}%%     ~~Describe \code{warn.conflicts} here~~
}
  \item{character.only}{
\code{\link{require}}%%     ~~Describe \code{character.only} here~~
}
  \item{file}{
character
}
}
\details{
Just as require, but quiet.
}
\value{
  Logical, invisibly.
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{require}}
}
\examples{
\dontrun{qrequire("zoo")}
}

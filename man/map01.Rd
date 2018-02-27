\name{map01}
\alias{map01}
\title{
  Map Numeric Values to Range
}
\description{
  Rescale a numeric vector in such a way that all values lie within a
  specified range (per default 0 to 1).
}
\usage{
map01(x, min = 0, max = 1, omin = min(x), omax = max(x), na.rm = FALSE)
}
\arguments{
  \item{x}{
    a numeric vector
}
  \item{min}{
    a numeric vector of length one
}
  \item{max}{
    a numeric vector of length one
}
  \item{omin}{
    a numeric vector of length one
}
  \item{omax}{
    a numeric vector of length one
}
  \item{na.rm}{
    logical
}
}
\details{

  Maps the elements of a numeric vector to a specified
  range; default is 0 to 1.
  
}
\value{
  a numeric vector 
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{scale}}
}
\examples{
map01(0:10)          ## 0, 0.1, 0.2 ... 1.0
map01(0:10, 0, 100)  ## 0, 10, 20 ... 100
}

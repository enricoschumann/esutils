\name{nth}
\alias{nth}
\title{
  Every Nth Element of a Vector
}
\description{
  Every nth element of a vector.
}
\usage{
nth(x, n, first = 1L)
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

  Subsets are chosen with \code{\link{Extract}}, so for a list a new
  list is returned.
  
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

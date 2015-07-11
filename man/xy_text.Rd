\name{xy_text}
\alias{xy_text}
\title{
  Add Text to Plot at Relative Coordinates
}
\description{

  Add  text to a plot at specific relative coordinates, e.g. 50\%, 50\%.

}
\usage{
xy_text(x, y, labels, ...) 
}
\arguments{
  \item{x}{
    numeric
  }
  \item{y}{
    numeric
  }
  \item{labels}{
    string
  }
  \item{\dots}{
    arguments passed to \code{\link{text}}
  }
}
\details{

  See \code{\link{text}}.
  
}
\value{
  A vector of coordinates, returned invisibly.
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{text}}
}
\examples{
plot(0:10, 0:10)
lines(5,5, col = "blue", pch = 19, type = "p")
xy_text(0.5,0.5, "The Centre", pos = 1)
xy_text(0.5,0.5, "The Centre", pos = 2)
xy_text(0.5,0.5, "The Centre", pos = 3)
xy_text(0.5,0.5, "The Centre", pos = 4)
}

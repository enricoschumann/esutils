\name{convertDate}
\alias{convertDate}
\title{
  Convert Various Formats to Dates
}
\description{
  Convert dates in external format (e.g., MATLAB) into Dates.
}
\usage{
convertDate(x, type, fraction = FALSE)
}
\arguments{
  \item{x}{
    numeric
  }
  \item{type}{
    character
  }
  \item{fraction}{
    logical
  }
}
\details{
  Convert 
}
\value{
a vector of class \code{\link{Date}}
}
\author{
  Enrico Schumann
}
\seealso{
\code{\link{as.Date}}
}
\examples{
convertDate(40000, "excel")
}

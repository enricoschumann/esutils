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
    character: currently \code{"excel"} and \code{"matlab"} are supported.
  }
  \item{fraction}{
    logical: should fractional dates (ie, times) be used?  Default is \code{FALSE}.
  }
}
\details{

  Convert the numeric representation of a date to class
  \code{\link{Date}}.  For Excel, only the Windows format is supported.
  Note that dates before 1 Mar 1900 are probably wrong since Excel
  considers 1900 a leap year (which it is not).

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

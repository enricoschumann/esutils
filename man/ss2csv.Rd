\name{ss2csv}
\alias{ss2csv}
\title{
  Spreadsheet to CSV
}
\description{
  Spreadsheet to CSV  
}
\usage{
ss2csv(file, out, path.exec = "ssconvert", \dots)
}

\arguments{
  \item{file}{
    the file to be converted. Note that the file is not actually changed.
}
  \item{out}{
    the outfile
}
  \item{path.exec}{
    path to ssconvert
}
  \item{\dots}{
    passed to \code{\link{system}}
}
}
\details{
  Relies on ssconvert, which ships with Gnumeric.
}
\value{
  The return value of system (0 signals success).
}
\references{
  \url{http://projects.gnome.org/gnumeric/}

  \url{http://projects.gnome.org/gnumeric/doc/sect-files-ssconvert.shtml}
}
\author{
  Enrico Schumann
}
\examples{
\dontrun{
## runs in the current directory
ss2csv("Journal.ods")}
}

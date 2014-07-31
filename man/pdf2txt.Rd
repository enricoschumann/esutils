\name{pdf2txt}
\alias{pdf2txt}
\title{
  Pdf to Text File
}
\description{
  Pdf to Text File  
}
\usage{
pdf2txt(file, out, path.exec = "pdftotext", \dots)
}

\arguments{
  \item{file}{
    the file to be converted. Note that the file is not actually changed.
}
  \item{out}{
    the outfile
}
  \item{path.exec}{
    path to pdftotext
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

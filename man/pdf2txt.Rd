\name{pdf2txt}
\alias{pdf2txt}
\title{
  Pdf to Text File
}
\description{
  Pdf to Text File  
}
\usage{
pdf2txt(file, out, path.exec = "pdftotext", \dots, layout = TRUE)
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
  \item{layout}{
    logical; defaults to \code{TRUE}
}
}
\details{
  Relies on ssconvert, which ships with Gnumeric.
}
\value{
  The return value of system (0 signals success).
}
\references{
  \url{http://www.foolabs.com/xpdf/home.html}
}
\author{
  Enrico Schumann
}
\examples{
\dontrun{
## runs in the current directory
pdf2txt("test.pdf")}
}

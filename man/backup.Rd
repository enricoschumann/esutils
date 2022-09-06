\name{backup}
\alias{old_files}
\alias{path8.3}
\alias{backup_filename}
\title{
  Tools for Creating Backups
}
\description{
  Functions for creating and maintaining backups.
}
\usage{
    old_files(min.age = 365,
              path = ".",
              pattern = NULL,
              ignore.case = FALSE,
              recursive = FALSE,
              min.age.monthend = NULL,
              min.age.yearend = NULL,
              full.names = FALSE)
    path8.3(path = ".")
    backup_filename(root, UTC = FALSE, \dots)
}
\arguments{
  \item{min.age}{
    number of days: an integer
  }
  \item{path}{
    a string
  }
  \item{pattern}{
    a string
  }
}
\details{

  \code{old_files} will look at filenames and then use
  \code{\link[datetimeutils]{guess_datetime}} to extract the date.


}
\value{
  for  \code{old_files}, a character vector
}
\author{
  Enrico Schumann
}
\examples{
backup_filename("daily_report.pdf")
}

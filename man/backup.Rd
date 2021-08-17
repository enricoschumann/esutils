\name{backup}
\alias{old_files}
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
  a character vector
}
\author{
  Enrico Schumann
}
%% \seealso{
%%   \code{\link{Extract}}
%% }
\examples{
}

\name{news_to_rss}
\alias{news_to_rss}
\alias{rss_feed}
\concept{RSS}
\concept{NEWS}
\title{
  Convert NEWS File to RSS Feed
}
\description{
  Convert package NEWS file to RSS items.
}
\usage{
news_to_rss(package, file,
            guid = function(version, date, ...) paste0(package, "_", version),
            guid.isPermaLink = FALSE, \dots)

rss_feed(items, title, description, language, link, atom.href, copyright)
}
\arguments{
  \item{package}{
    character
}
  \item{file}{
    character
}
  \item{guid}{
    function
}
  \item{guid.isPermaLink}{
    logical
}
  \item{\dots}{
    more arguments
}
}
\details{

%  TODO
  
}
\value{
  character vector
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link[utils]{news}}
}
\examples{
\dontrun{

copyright <- "Copyright 2015, Enrico Schumann"
title <- "NMOF – Numerical Methods and Optimization in Finance"
link <- "http://enricoschumann.net/R/packages/NMOF/"

description <- "News about package NMOF"
language <- "en-gb"
atom.href <- "http://enricoschumann.net/R/packages/NMOF/NMOF_news.xml"


cat(rss_feed(news_to_rss("NMOF", link = link), title, description,
             language, link, atom.href, copyright),
    file = "~/tmp/NMOF_news.xml")

system("xmllint -o ~/ESdotnet/R/packages/NMOF/NMOF_news.xml --format ~/tmp/NMOF_news.xml")


}}

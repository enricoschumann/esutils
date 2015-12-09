\name{news_to_rss}
\alias{news_to_rss}
\concept{RSS
\title{
  Convert NEWS to RSS
}
\description{
  Convert package NEWS file to RSS items.
}
\usage{
news_to_rss(package, file,
            guid = function(version, date, ...) paste0(package, "_", version),
            guid.isPermaLink = FALSE, \dots)
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

  Exp.
  
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

head <-
'<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>NMOF – Numerical Methods and Optimization in Finance</title> 
    <link>http://enricoschumann.net/R/packages/NMOF/</link> 
    <description>News about package NMOF</description> 
    <language>en-gb</language> 
    <copyright>Copyright 2015, Enrico Schumann</copyright>
    <atom:link href="http://enricoschumann.net/R/packages/NMOF/NMOF_news.xml" rel="self" type="application/rss+xml" />'

foot <-
'</channel>
</rss>'


rss_header <- function(title, description,
                       language, link,
                       atom.href, copyright) {

    head <-
'<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
    <title>#title#</title> 
    <link>#link#</link> 
    <description>#description#</description> 
    <language>#language#</language> 
    <copyright>#copyright#</copyright>
    <atom:link href="#atom.href#"
               rel="self" type="application/rss+xml" />'

    for (s in c("title", "description", "language", "link",
                "atom.href", "copyright")) {
        head <- gsub(paste0("#", s, "#"), get(s), head)
    }
    head
}

copyright <- "Copyright 2015, Enrico Schumann"
title <- "NMOF – Numerical Methods and Optimization in Finance"
link <- "http://enricoschumann.net/R/packages/NMOF/"

description <- "News about package NMOF"
language <- "en-gb"
atom.href <- "http://enricoschumann.net/R/packages/NMOF/NMOF_news.xml"

## rss
cat(c(rss_header(title, description,
             language, link,
             atom.href, copyright),
  news_to_rss("NMOF"),
  '</channel>
</rss>'),file = "~/tmp/NMOF_news.xml")


## package <- "NMOF"
## packageDescription(package)$Title
## file <- "~/Packages/NMOF/NEWS"

## cat(head, items, foot, file = "~/tmp/NMOF_news.xml")
system("xmllint -o ~/ESdotnet/R/packages/NMOF/NMOF_news.xml --format ~/tmp/NMOF_news.xml")

}}

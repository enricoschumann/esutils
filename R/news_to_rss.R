## -*- truncate-lines: t; fill-column: 65; comment-column: 50; -*-

## require("datetimeutils")
## require("textutils")


## guid <- function(version, date, ... ) {
##     paste0(package, "_", version)
## }

news_to_rss  <- function(package, file,
                         guid = function(version, date, ...)
                                    paste0(package, "_", version),
                         guid.isPermaLink = FALSE,
                         ... ) {

    ns <- if (!missing(file))
              tools:::.news_reader_default(file)
          else
              news(package = package)

    ## ans <- aggregate(trim(ns$Text), by = list(ns$Version),
    ##                  FUN = function(x)
    ##                            paste0("&lt;p&gt;", x, "&lt;/p&gt;",
    ##                                   collapse = "\n"))

    ans <- aggregate(trim(ns$Text),
                     by = list(ns$Version),
                     FUN = function(x)
                             paste0("&lt;li&gt;", x, "&lt;/li&gt;",
                                    collapse = "\n"))

    dates <- aggregate(ns$Date,
                       by = list(ns$Version),
                       FUN = tail, 1)

    ## browser()
    g <- guid(dates[[1]], dates[[2]], ...)
    items  <- paste0("<item>\n",
                     "<title>", ans[[1]],"</title>\n",
                     "<link>http://enricoschumann.net/R/packages/NMOF</link>\n",
                     ifelse(is.na(dates[[2]]), "", paste0("<pubDate>", rfc822t(dates[[2]]) ,"</pubDate>\n")),
                     "<guid isPermaLink=\"false\">", g , "</guid>\n",
                     "<description>\n&lt;ul&gt;\n", ans[[2]], "\n&lt;/ul&gt;\n</description>\n",
                     "</item>\n")
    items
}

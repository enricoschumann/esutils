## -*- truncate-lines: t; -*-
## Time-stamp: <2014-07-04 17:52:42 CEST (es)>

test.convertDate <- function() {
    require("esTools")
    checkEquals(convertDate(41824, "excel"), as.Date("2014-07-04"))
    checkEquals(convertDate(61, "excel"), as.Date("1900-03-1"))
    checkEquals(convertDate(61, "excel"), as.Date("1900-03-1"))

}

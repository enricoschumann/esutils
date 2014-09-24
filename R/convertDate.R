convertDate <- function(x, type, fraction = FALSE, tz = "") {
    type <- tolower(type)
    if (type == "excel" && !fraction){
        as.Date(x, origin = "1899-12-30")
    } else if (type == "excel") {
        tmp <- as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC")
        as.POSIXct(strptime(format(tmp), format = "%Y-%m-%d %H:%M:%S", tz = tz))        
    } else if (type == "matlab" && !fraction) {
        as.Date(x, origin = "1970-01-01") - 719529
    } else if (type == "matlab" && fraction) {
        tmp <- as.POSIXct((x - 719529) * 86400, origin = "1970-01-01")
        as.POSIXct(strptime(format(tmp), format = "%Y-%m-%d %H:%M:%S", tz = tz))        
    } else
        stop("unknown type")
}

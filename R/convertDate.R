convertDate <- function(x, type, fraction = FALSE) {
    type <- tolower(type)
    if (type == "excel" && !fraction){
        as.Date(x, origin = "1899-12-30")              
    } else if (type == "matlab" && !fraction) {
        as.Date(x, origin = "1970-01-01") - 719529
    } else if (type == "matlab" && fraction) {
        as.POSIXct((x - 719529) * 86400, origin = "1970-01-01")
    } else
        stop("unknown type")
}

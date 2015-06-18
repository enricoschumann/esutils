localTesting <- TRUE
pgk <- "esutils"
if (require("RUnit", quietly = TRUE)) {
    require(pkg)
    if (localTesting)
        path <- paste0("~/Packages/", pkg, "/inst/unitTests" else
    path <- system.file("unitTests", package = pkg)
    myTestSuite <- defineTestSuite(pkg,
                                   dirs = path,
                                   testFileRegexp = "ut_.*")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}

localTesting <- TRUE
if (require("RUnit", quietly = TRUE)) {
    require("esTools")
    if (localTesting)
        path <- "~/Packages/esTools/inst/unitTests" else
    path <- system.file("unitTests", package = "esTools")
    myTestSuite <- defineTestSuite("esTools",
                                   dirs = path,
                                   testFileRegexp = "ut_.*")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}

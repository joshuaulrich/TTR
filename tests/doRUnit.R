## unit tests will not be done if RUnit is not available
if(require("RUnit", quietly=TRUE)) {
 
  ## --- Setup ---
  R_CMD_CHECK <- Sys.getenv("RCMDCHECK") != "FALSE"
 
  pkg <- "TTR" # <-- Change to package name!

  if (R_CMD_CHECK) {
    ## Path to unit tests for R CMD check
    ## PKG.Rcheck/PKG/unitTests
    path <- system.file("unitTests", package=pkg)
  } else {
    ## Path to unit tests for standalone running under Makefile (not R CMD check)
    ## PKG/tests/../inst/unitTests
    path <- file.path(getwd(), "..", "inst", "unitTests")
  }
  cat("\nRunning unit tests\n")
  print(list(pkg=pkg, getwd=getwd(), pathToUnitTests=path))
 
  library(package=pkg, character.only=TRUE)
 
  ## If desired, load the name space to allow testing of private functions
  ## if (is.element(pkg, loadedNamespaces()))
  ##     attach(loadNamespace(pkg), name=paste("namespace", pkg, sep=":"), pos=3)
  ##
  ## or simply call PKG:::myPrivateFunction() in tests
 
  ## --- Testing ---
 
  ## Define tests
  testSuite <- defineTestSuite(name=paste(pkg, "unit testing"),
                                          dirs=path)
  ## Run
  tests <- runTestSuite(testSuite)

  ## Report to stdout
  cat("------------------- UNIT TEST SUMMARY ---------------------\n\n")
  printTextProtocol(tests, showDetails=FALSE)

  ## Report text files (only if not under R CMD check)
  if (!R_CMD_CHECK) {
    ## Default report name
    pathReport <- file.path(path, "report")

    printTextProtocol(tests, showDetails=FALSE,
                      fileName=paste(pathReport, "Summary.txt", sep=""))
    printTextProtocol(tests, showDetails=TRUE,
                      fileName=paste(pathReport, ".txt", sep=""))

    ## Report to HTML file
    printHTMLProtocol(tests, fileName=paste(pathReport, ".html", sep=""))
  }

  ## Return stop() to cause R CMD check stop in case of
  ##  - failures i.e. FALSE to unit tests or
  ##  - errors i.e. R errors
  testErrors <- getErrors(tests)
  if(testErrors$nFail > 0) {
    msg <- paste0(" unit test", if(testErrors$nFail > 1) "s" else "", " failed")
    stop("\n", testErrors$nFail, msg, sep="")
  }
  if(testErrors$nErr > 0) {
    msg <- paste0(" unit test", if(testErrors$nErr > 1) "s" else "", " had errors")
    stop("\n", testErrors$nErr, msg, sep="")
  }
  if (testErrors$nTestFunc < 1) {
    stop("No test functions ran!")
  }
} else {
  warning("cannot run unit tests -- package RUnit is not available")
}

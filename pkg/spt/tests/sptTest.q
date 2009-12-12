rm(list=ls()); gc()
source("sptLoad.q")
library(RUnit)

## Tidy up the test files to prevent old test
## files from accidentally being run.
#testFiles <- list.files("~/Files/EPA/Projects/Libraries/spatiotemporal/spttest")
testFiles <- list.files("sptUnitTests")
tmp.files <- grep(".q~",testFiles)
if (length(tmp.files)>0){
  system("rm sptUnitTests/*.q~",intern=TRUE)
}

testsuite.spt <- defineTestSuite("spt", dirs="sptUnitTests",
                                 testFileRegexp="^runit.+.q",
                                 testFuncRegexp="^test.+")
testResult <- runTestSuite(testsuite.spt)

## Open the unit test results in firefox (on windows).
fname <- "sptUnitTestResults.html"
printHTMLProtocol(testResult, fileName=fname)
reportUrl <- paste("file:/",getwd(), fname,sep="/")
firefoxUrl <- paste(unlist(strsplit(reportUrl," ")), collapse="%20")

if (Sys.info()["sysname"] == "Windows") {
  browseURL( firefoxUrl,  browser="C:/Program Files/Mozilla Firefox/firefox.exe")
} else {
  browseURL(reportUrl)
}

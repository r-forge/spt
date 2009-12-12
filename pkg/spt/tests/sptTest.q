## Tidy up the test files to prevent old test
## files from accidentally being run.
testFiles <- list.files("~/Files/EPA/Projects/Libraries/spatiotemporal/spttest")
tmp.files <- grep(".q~",testFiles)
if (length(tmp.files)>0){
  system("rm ~/Files/EPA/Projects/Libraries/spatiotemporal/spttest/*.q~",intern=TRUE)
}

testsuite.spt <- defineTestSuite("spt", dirs="~/Files/EPA/Projects/Libraries/spatiotemporal/spttest",
                                 testFileRegexp="^runit.+.q",
                                 testFuncRegexp="^test.+")
testResult <- runTestSuite(testsuite.spt)

## Open the unit test results in firefox (on windows).
fname <- "/Users/blairc/Files/EPA/Projects/Libraries/spatiotemporal/spttest/temp.html"
printHTMLProtocol(testResult, fileName=fname)
reportUrl <- paste("file:/",fname,sep="/")
firefoxUrl <- paste(unlist(strsplit(reportUrl," ")), collapse="%20")

if (Sys.info()["sysname"] == "Windows") {
  browseURL( firefoxUrl,  browser="C:/Program Files/Mozilla Firefox/firefox.exe")
} else {
  browseURL(reportUrl)
}

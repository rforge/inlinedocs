#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
source("isolatedTestRunner.R")
tmpDir="tmp"
arg=commandArgs(trailingOnly=TRUE)
if (length(arg)>0) {
  tmpDir <- arg[[1]]
}
runIsolatedTests(
  srcDir=file.path(getwd(),"../../../R/"),
  tmpDir=tmpDir,
  dirs=".",
  testFileRegexp="^runit.+\\.R",
  testFuncRegexp = "^test.+"
)                 

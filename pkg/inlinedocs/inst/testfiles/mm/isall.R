#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
source("isolatedTestRunner.R")
runIsolatedTests(
  dirs=".",
  testFileRegexp="^runit.+\\.R",
  testFuncRegexp = "^test.+"
)                 

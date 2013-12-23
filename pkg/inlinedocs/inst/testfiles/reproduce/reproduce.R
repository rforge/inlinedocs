#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
source("prolog.R")
alltests <- defineTestSuite(
   name="allTests",
   dirs=".",
   testFileRegexp = "^runit\\.reproduceError.+\\.[rR]",
   testFuncRegexp = "^test.+",
   #rngKind = "Marsaglia-Multicarry",
   #rngNormalKind = "Kinderman-Ramage"
)

testResult <- runTestSuite(alltests)
printTextProtocol(testResult)

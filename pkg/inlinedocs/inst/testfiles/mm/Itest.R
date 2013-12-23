#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
source("isolatedTestRunner.R")
runIsolatedTests(
  dirs=".",
  #testFileRegexp = "^runit.Infrastructure.R",
  #testFileRegexp = "^runit.OperatorDoc.R",
  #testFileRegexp = "^runit.NamespaceExample.R",
  testFileRegexp="^runit.MethodDoc.R",
  #testFileRegexp="^runit.AddExampleCodeFromExternalTest.R",
  
  testFuncRegexp = "^test.MethodDescription"
  #testFuncRegexp = "^test.inherit.docs"
  #testFuncRegexp = "^test.extract.file.parse.ClassParents"
  #testFuncRegexp = "^test.extract.file.parse.MethodParents"
  #testFuncRegexp = "^test.OperatorDescription"
  #testFuncRegexp = "^test.EmptyOperatorRdFiles"
  #testFuncRegexp="^test.ExtraMethodDocumentationFile"
  #testFuncRegexp="^test.AddExampleCodeFromExternalTest"
  #testFuncRegexp="^test.AddExampleCodeForMethodsFromExternalTest"
)                 

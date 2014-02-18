#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
source("isolatedTestRunner.R")
tmpDir="tmp"
arg=commandArgs(trailingOnly=TRUE)
if (length(arg)>0) {
  tmpDir <- arg[[1]]
}
  
testFuncRegexp <- 
#"test.completeMethodDescriptionByCommentAndTag"
#"test.completeMethodDocArgList"
#"test.completeFunctionDocArgList"
#"test.completeMethodTitleByComment"
#"test.completeFunctionDocDescriptionByCommentAndTags"
#"test.completeFunctionDocDescriptionByComment"
#"test.completeFunctionDocTitleByPrecedingCommentAndTags"
#"test.completeFunctionDocTitleByPrecedingComment"
#"test.completeFunctionDocTitleByFunctionName"
#"test.completeMethodDescription"
#"test.MethodDescription"
"test.inherit.docs"
#"test.extract.file.parse.ClassParents"
#"test.extract.file.parse.MethodParents"
#"test.EmptyOperatorRdFiles"
#"test.ExtraMethodDocumentationFile"
#"test.ParsersOnSingleMethods"
#"test.AddExampleCodeFromExternalTest"
#"test.AddExampleCodeForMethodsFromExternalTest"
#"test.AddExampleCodeForMethodsFromExternalTestIntoRdFile"
#"test.toLongUsageLine"
#"test.changedMethodsRdFilesForHiddenMethods"
#"test.noMethodRdFilesForHiddenMethods"
#"test.manualDoc"
#"test.packageBuild"
#"test.NameSpaceParsing"
#"test.BracketsForListOfMethods"

runIsolatedTests(
  #srcDir="/home/mm/SoilR/RPackages/inlinedocs/pkg/inlinedocs/R",
  srcDir=file.path(getwd(),"../../../R/"),
  dirs=".",
  tmpDir,
  testFileRegexp = "^runit.*",
  testFuncRegexp
)                 
p <- file.path(tmpDir, testFuncRegexp,"stderr")
system(paste("cat ",p))

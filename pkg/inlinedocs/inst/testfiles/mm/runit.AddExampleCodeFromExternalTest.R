test.AddExampleCodeFromExternalTest.extractExample=function(){
	options("inlinedocs.exampleRegExpression"="^examples\\.")
	pkdir="pkg"
	RDir=file.path(pkdir,"R")
	TestDir=file.path(pkdir,"inst","tests")
	dir.create(RDir,recursive=TRUE)
	dir.create(TestDir,recursive=TRUE)
	options("inlinedocs.exampleDir"=TestDir)
	code='
funcWithExternalExample<- function
### function for which an example should be added from a testfile for that function
( ){
  cat("funcWithExternalExample!\n")
  ### None (invisible NULL)
}
'	
	f=file.path(RDir,"funcWithExternalExample.R")
	cat(file=f,code)
	
	
	testCode='
funcWithExternalExample("blub")
	'
	cat(file=file.path(TestDir,"examples.funcWithExternalExample.R"),text=testCode)

	suppressWarnings(sys.source(f))
	parsers=NULL
	result <- extract.docs.file(f,parsers)
	pp("result",environment())

	checkTrue(CompareTrimmedNonEmptyLines(result$funcWithExternalExample$examples,"funcWithExternalExample(\"blub\")"))

}

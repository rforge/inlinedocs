#
# vim:set ff=unix expandtab ts=2 sw=2:
test.AddExampleCodeFromExternalTest.extractExample=function(){
	options("inlinedocs.exampleTrunk"="examples.")
	pkdir="pkg"
	RDir=file.path(pkdir,"R")
	TestDir=file.path(pkdir,"inst","tests")
	dir.create(RDir,recursive=TRUE)
	dir.create(TestDir,recursive=TRUE)
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
	cat(file=file.path(TestDir,"example.funcWithExternalExample.R"),text=testCode)

	suppressWarnings(sys.source(f))
	parsers=NULL
	result <- extract.docs.file(
		f,
		parsers,
		inlinedocs.exampleDir=TestDir,
	  inlinedocs.exampleRegExpression="example."
	)
	pp("result",environment())

	checkTrue(CompareTrimmedNonEmptyLines(result$funcWithExternalExample$examples,"funcWithExternalExample(\"blub\")"))

}

#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
summa=function(results){
  ne=0
  nf=0
  nt=0
  errorList=list()
  failList=list()
  for (res in results){
    if (res$nErr!=0){
      ne=ne+res$nErr
      name=res$testFuncRegexp
      errorList[[name]] <- res
    }
    if (res$nFail!=0){
      nf=nf+res$nFail
      name=res$testFuncRegexp
      failList[[name]] <- res
    }
    nt=nt+res$nTestFunc
  }
  print(paste("The number of test function:",nt))
  if (ne >0){
	print("########## errors ############")
	print(paste("number of errors:",ne))
        print(names(errorList))
  }
  if (length(failList)>0){
	print("########## failures  ############")
	print(paste("number of failures:",nf))
	print(names(failList))
  }
}
################################################
runIsolatedTests <- function(
  dirs,
  testFileRegexp,
  testFuncRegexp
){
  require("RUnit")
  e <- new.env()
  #dirs="."
  #testFileRegexp="^runit.MethodDoc.R"
  #testFuncRegexp="^test.ExtraMethodDocumentationFile"
  testFiles <- list.files(dirs, pattern = testFileRegexp, full.names = TRUE)
  for (testFile in testFiles) {
    sys.source(testFile,e,keep.source=TRUE)
  }
  
  srcFileName="AutomaticTestSource.R"
  prolog='require(methods)
  require("RUnit")
  prefix="../../../../../R"
  auto_paths=Sys.glob(paste(prefix,"*.R",sep="/"))
  for (f in auto_paths){
      source(f,echo=FALSE)
  }'
  print(ls(e))
  results=list()
  testFunctions <- ls(pattern = testFuncRegexp, envir=e)
  for (fun in testFunctions){
  	src=attr(e[[fun]],"srcref")
    testcode=paste(fun,"<-",paste(src,collapse="\n"),sep="")
    code=paste(prolog,testcode,sep="\n")
    dir=file.path("tmp",fun)
    unlink(dir,recursive=TRUE,force=TRUE)
    dir.create(dir,recursive=TRUE)
    pwd=setwd(dir)
    cat(code,file=srcFileName)
  	run=paste(
  	'require("RUnit")
  	singleTest <- defineTestSuite(
  	   name="iso",
  	   dirs=".",
  	   testFileRegexp ="', srcFileName,'",
  	   testFuncRegexp = "',fun,'",
  	)
  	
  	testResult <- runTestSuite(singleTest)
  	save(testResult,file="testResult")
  	',sep="")
  	cat(run,file="run.R")
  	res=system("Rscript run.R")
  	print(res)
  	checkEquals(res,0,"error in script")
    load("testResult")
    results=append(results,testResult)
    setwd(pwd)
  }
  summa(results)
}

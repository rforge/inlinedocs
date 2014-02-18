#!/usr/bin/Rscript
# vim:set ff=unix expandtab ts=2 sw=2:
summa=function(results){
  ne=0
  nf=0
  nt=0
  errorList=list()
  failList=list()
  for (testFunName in names(results)){
    result=results[[testFunName]]
    errors=getErrors(result)
    if (errors$nErr!=0){
      ne=ne+errors$nErr
      errorList[[testFunName]] <- errors
    }
    if (errors$nFail!=0){
      nf=nf+errors$nFail
      failList[[testFunName]] <- errors
    }
    nt=nt+errors$nTestFunc
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
  srcDir, 
  tmpDir="tmp",
  dirs=".",
  testFileRegexp,
  testFuncRegexp
  ){
  require("RUnit")
  require("parallel")
  e <- new.env()

  testFiles <- list.files(dirs, pattern = testFileRegexp, full.names = TRUE)
  for (testFile in testFiles) {
    sys.source(testFile,e,keep.source=TRUE)
  }
  
  srcFileName="AutomaticTestSource.R"
  prolog=paste('require(methods)
  require("RUnit")
  prefix="',srcDir,'"
  auto_paths=Sys.glob(paste(prefix,"*.R",sep="/"))
  for (f in auto_paths){
      source(f,echo=FALSE)
  }',sep="")
  #print(ls(e))
  results=list()
  testFunctions <- ls(pattern = testFuncRegexp, envir=e)
  runSingleTest <- function(fun){
  	src=attr(e[[fun]],"srcref")
    testcode=paste(fun,"<-",paste(src,collapse="\n"),sep="")
    code=paste(prolog,testcode,sep="\n")
    dir=file.path(tmpDir,fun)
    unlink(dir,recursive=TRUE,force=TRUE)
    dir.create(dir,recursive=TRUE)
    #pwd=setwd(dir)
    cat(code,file=file.path(dir,srcFileName))
  	run=paste(
  	'require("RUnit")
    setwd("',dir,'")
  	singleTest <- defineTestSuite(
  	   name="iso",
  	   dirs=".",
  	   testFileRegexp ="',srcFileName,'",
  	   testFuncRegexp = "',fun,'",
  	)
  	
  	testResult <- runTestSuite(singleTest)
  	save(testResult,file="testResult")
  	',sep="")
  	cat(run,file=file.path(dir,"run.R"))
  	command <- paste(
      "Rscript",
      file.path(dir,"run.R"),
      ">",
      file.path(dir,"stdout"),
      "2 >",
      file.path(dir,"stderr"),
      collapse=" "
    )
    print(command)
    res <- system(command)
  	#print(res)
  	checkEquals(res,0,"error in script")
    load(file.path(dir,"testResult"))
    testResult
  }
  results <- mclapply(testFunctions,runSingleTest,mc.cores=32)
  names(results) <- testFunctions
  summa(results)
}

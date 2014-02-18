#
# vim:set ff=unix expandtab ts=2 sw=2:
test.toLongUsageLine=function(){
	pkgDir="pkg"
	RDir=file.path(pkgDir,"R")
	TestDir=file.path(pkgDir,"inst","tests")
	dir.create(RDir,recursive=TRUE)
	#dir.create(TestDir,recursive=TRUE)
 srcCode='
#################################################
# define a simple function with too many arguments
#################################################
#################################################
functionWithARatherLongAndTiresomeNameOnlyExcusableByItsPurpose=function# hidden square
###  The function squares its argument
(
 FirstArgumentWithVeryLongName,  ##<< a number
 SecondArgumentWithVeryLongName, ##<< a number
 ThirdArgumentWithVeryLongName,  ##<< a number
 ForthArgumentWithVeryLongName,  ##<< a number
 FifthArgumentWithVeryLongName,  ##<< a number
 x 
){
   return(x^2)
}

ParallelModel=function
### This function creates a numerical model for n independent (parallel) pools that can be queried afterwards. 
(times,		##<< A vector containing the points in time where the solution is sought.
 coeffs_tm,	##<< A TimeMap object consisting of a vector valued function containing the decay rates for the n pools as function of time and the time range where this function is valid. The length of the vector is equal to the number of pools.
 startvalues,	##<< A vector containing the initial amount of carbon for the n pools. 
 ##<<The length of this vector is equal to the number of pools and thus equal to the length of k. This is checked by the function.
 inputrates, ##<< A TimeMap object consisting of a vector valued function describing the inputs to the pools as funtions of time
 solverfunc =4,    ##<< The function used to actually solve the ODE system. This can be 
  pass=FALSE  ##<< if TRUE forces the constructor to create the model even if it is invalid 
 ){
}

'
  f=file.path(RDir,"source.R")
	cat(file=f,srcCode)
pkgName='Example'  
pkgVersion='0.0-1'  
desc <-paste("
Package:",pkgName," 
Title: Examples 
Version:",pkgVersion,"
Date: 2013-03-4
Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
Description: This package contains some functions to be tested
License: GPL-3
Depends:methods,RUnit 
",sep="")

  descFilePath=file.path(pkgDir,"DESCRIPTION")
	cat(file=descFilePath,text=desc)
namesp <- ' export(
functionWithARatherLongAndTiresomeNameOnlyExcusableByItsPurpose)
'
  NamespaceFilePath=file.path(pkgDir,"NAMESPACE")
	cat(file=NamespaceFilePath,text=namesp)
	parsers=NULL
  #result <- extract.docs.file(f,parsers) #[["exposedFunction"]]
	#pp("result",environment())
  #package.skeleton.dx(pkgDir)
  tryCatch(package.skeleton.dx(pkgDir))
  res=system(paste("R CMD build",pkgDir))
 	checkEquals(res,0,"error in R CMD build")
  returnValue=system(paste("R CMD check --as-cran ",pkgDir))
 	checkEquals(returnValue,0,"error in R CMD check ")


  cf=paste(pkgDir,".Rcheck/00check.log",sep="")
  cfl=readLines(cf)
  checkTrue(!any(grepl("warning",cfl,ignore.case = TRUE)))
  pe(quote(length(grep("NOTE",cfl,ignore.case = TRUE))),environment())
  checkTrue(length(grep("NOTE",cfl,ignore.case = TRUE))<3)#this means one not
  ##which is actually mentioned twice in the file

}


#
# vim:set ff=unix expandtab ts=2 sw=2:
test.EmptyOperatorRdFiles=function(){
	pkgDir="pkg"
	RDir=file.path(pkgDir,"R")
	TestDir=file.path(pkgDir,"inst","tests")
	dir.create(RDir,recursive=TRUE)
	dir.create(TestDir,recursive=TRUE)
 srcCode='
#################################################
setClass(# HiddenClass
   Class="HiddenClass",
   representation=representation(
        times="numeric"
   )
)

#################################################
setClass(#ExposedClass
   Class="ExposedClass",
   representation=representation(
        times="numeric"
   )
)
#################################################
# overload the [[ operator which is done only here.
# but since the class is hidden the cooresponding Method desription file z-[[-methods would be empty
# so preferably it should disappear completely
# (template created by: method.skeleton("[[","HiddenClass")
setMethod("[[",
    signature = "HiddenClass",
    function (x, i, j, ...) 
    {
        print("I am a hidden method because I belong to a class which is not exported in the NAMESPACE File")
    }
)
'
  f=file.path(RDir,"source.R")
	cat(file=f,srcCode)
desc <-'
Package: NamespaceExample
Title: Examples to test the possibilities of Namespaces  
Version: 0.0-1
Date: 2013-03-4
Author:  Markus Mueller <mamueller@bgc-jena.mpg.de>
Maintainer: Markus Mueller <mamueller@bgc-jena.mpg.de>
Description: This package contains some functions to be tested
License: GPL-3
Depends:methods,RUnit 
'
  descFilePath=file.path(pkgDir,"DESCRIPTION")
	cat(file=descFilePath,text=desc)
namesp <- '
exportClasses(
ExposedClass
)
'
  NamespaceFilePath=file.path(pkgDir,"NAMESPACE")
	cat(file=NamespaceFilePath,text=namesp)
	parsers=NULL
  #tryCatch(package.skeleton.dx(pkgDir))
  result <- extract.docs.file(f,parsers)
	##pp("parsed",environment())
	#pp("result",environment())

}


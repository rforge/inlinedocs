#
# vim:set ff=unix expandtab ts=2 sw=2:
test.AddExampleCodeForMethodsFromExternalTest=function(){
	pkgDir="pkg"
	RDir=file.path(pkgDir,"R")
	TestDir=file.path(pkgDir,"inst","tests")
	dir.create(RDir,recursive=TRUE)
	dir.create(TestDir,recursive=TRUE)
	srcCode='
# Minimum code exampe to test method documentation
# vim:set ff=unix expandtab ts=2 sw=2:
########################################################
require("methods")
nonGenricFunc=function(# fsd
  ### fld
  x ##<< fa1
  )
{
  x
  ##<< fv
}
setGeneric(
  name="GenericFunc",
  def=function(# A short description of the generic function
    ### This function is generic
    .Object, ##<< d2 
    num 
    ){standardGeneric("GenericFunc")
  ##value<< d3
  }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="A",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="initialize",
    signature="A",
    definition=function(.Object,name){
      .Object@name <- name
      return(.Object)
    }
)
########################################################
setMethod(
    f="GenericFunc",
    signature=c("A","numeric"),
    definition=function(# A short description of the method for class A
        ### This method deals with objects of class A
       .Object, ##<< an object of class "A"
       num
       ){
       return(.Object@name) 
    }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="B",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="initialize",
    signature="B",
    definition=function(.Object,name,value){
      .Object@name <- name
      return(.Object)
    }
)
########################################################
setMethod(
    f="GenericFunc",
    signature=c("B","character"),
    definition=function(# A short description of the method for class B
        ### This method deals with objects of class B
       .Object, ##<< an object of class "B"
       num 
       ){
       return(.Object@name) 
    }
)
'
  f=file.path(RDir,"source.R")
	
  cat(file=f,srcCode)
  testCodeA='
test.A(){
        reqire("RUnit")
	o=new("A","myname")
	print(GenericFunc(o,4))
	print(isGeneric("GenericFunc"))
} 
'
	cat(file=file.path(TestDir,"examples.GenericFunc-method-#A#numeric.R"),text=testCodeA)
  testCodeB='
test.B(){
        reqire("RUnit")
	o=new("B",1)
	print(GenericFunc(o,"blub"))
	print(isGeneric("GenericFunc"))
} 
'
	cat(file=file.path(TestDir,"examples.GenericFunc-method-#B#character.R"),text=testCodeB)
	options("inlinedocs.exampleRegExpression"="^examples\\.")
	
  parsers=NULL
	options("inlinedocs.exampleDir"=TestDir)
	result <- extract.docs.file(f,parsers)
	#pp("result",environment())
	checkTrue(CompareTrimmedNonEmptyLines(result[["GenericFunc-method-#A#numeric"]]$examples,testCodeA))
	checkTrue(CompareTrimmedNonEmptyLines(result[["GenericFunc-method-#B#character"]]$examples,testCodeB))
}
############################################################################################################################
test.MethodDescription=function(){
    require("stringr")
    code='  
########################################################
require("methods")
setGeneric(
  name="GenericFunc",
  def=function(# A short description of the generic function
    ### This function is generic
    .Object, ##<< d2 
    num 
    ){standardGeneric("GenericFunc")
  ##value<< d3
  }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="A",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="GenericFunc",
    signature=c("A","numeric"),
    definition=function(# A short description of the method for class A
        ### This method deals with objects of class A
       .Object, ##<< an object of class "A"
       num ##<< a number
       ){
       return(.Object@name) 
       ##value<< d3
    }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="B",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="GenericFunc",
    signature=c("B","character"),
    definition=function(# A short description of the method for class B
        ### This method deals with objects of class B
       .Object, ##<< an object of class "B"
       num 
       ){
       return(.Object@name) 
       ##value<< f3
    }
)
    '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)
#	
#  
#	#pp("result",environment())
#
#	checkEquals(result[["GenericFunc-method-#A#numeric"]][["description"]],"This method deals with objects of class A")
#	checkEquals(result[["GenericFunc-method-#B#character"]][["description"]],"This method deals with objects of class B")
#	checkEquals(result[["GenericFunc-method-#A#numeric"]][["description"]],"This method deals with objects of class A")
#	checkEquals(result[["GenericFunc-method-#B#character"]][["description"]],"This method deals with objects of class B")
#	checkEquals(result[["GenericFunc-method-#A#numeric"]][["title"]],"A short description of the method for class A")
#	checkEquals(result[["GenericFunc-method-#B#character"]][["title"]],"A short description of the method for class B")
}
############################################################################################################################
test.ExtraMethodDocumentationFile=function(){
  require("stringr")
	pkgDir="pkg"
	RDir=file.path(pkgDir,"R")
	TestDir=file.path(pkgDir,"inst","tests")
	dir.create(RDir,recursive=TRUE)
	dir.create(TestDir,recursive=TRUE)
	srcCode='
# Minimum code exampe to test method documentation
# vim:set ff=unix expandtab ts=2 sw=2:
########################################################
require("methods")
nonGenricFunc=function(# fsd
  ### fld
  x ##<< fa1
  )
{
  x
  ##<< fv
}
setGeneric(
  name="GenericFunc",
  def=function(# A short description of the generic function
    ### This function is generic
    .Object, ##<< d2 
    num 
    ){standardGeneric("GenericFunc")
  ##value<< d3
  }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="A",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="initialize",
    signature="A",
    definition=function(.Object,name){
      .Object@name <- name
      return(.Object)
    }
)
########################################################
setMethod(
    f="GenericFunc",
    signature=c("A","numeric"),
    definition=function(# A short description of the method for class A
        ### This method deals with objects of class A
       .Object, ##<< an object of class "A"
       num
       ){
       return(.Object@name) 
    }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="B",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="initialize",
    signature="B",
    definition=function(.Object,name,value){
      .Object@name <- name
      return(.Object)
    }
)
########################################################
setMethod(
    f="GenericFunc",
    signature=c("B","character"),
    definition=function(# A short description of the method for class B
        ### This method deals with objects of class B
       .Object, ##<< an object of class "B"
       num 
       ){
       return(.Object@name) 
    }
)
'
  f=file.path(RDir,"source.R")
	cat(file=f,srcCode)
desc <-'
Package:MethodDoc 
Title: example of inlinedocs Documentation of Methods
Type: Package
Version: 1.0
Date:Mo 18. Nov 15:08:00 CET 2013 
Author: Markus Müller
Maintainer: Markus Müller <mamueller.bgc-jena.mpg.de>
Description: Attempt to exercise method documentation
URL: http://inlinedocs.r-forge.r-project.org
License: GPL-3
LazyLoad: yes
Depends: methods
'
  descFilePath=file.path(pkgDir,"DESCRIPTION")
	cat(file=descFilePath,text=desc)
	parsers=NULL
  manDir=file.path(pkgDir,"man")
  pp("manDir",environment())
  unlink(paste(manDir,"/*",sep=""),recursive=TRUE)
  #pwd=setwd(d)
  #tryCatch(package.skeleton.dx("MethodDoc"),finally=setwd(pwd))
  tryCatch(package.skeleton.dx(pkgDir))
  strA <- '
\\name{GenericFunc-method-#A#numeric}
\\alias{GenericFunc-method-#A#numeric}
\\title{A short description of the method for class A}
\\description{This method deals with objects of class A}
\\usage{fff(.Object, num)}
\\arguments{
  \\item{.Object}{an object of class "A"}
  \\item{num}{
}
}
'
  
  strB <- '
\\name{GenericFunc-method-#B#character}
\\alias{GenericFunc-method-#B#character}
\\title{A short description of the method for class B}
\\description{This method deals with objects of class B}
\\usage{fff(.Object, num)}
\\arguments{
  \\item{.Object}{an object of class "B"}
  \\item{num}{
}
}
  '
  refB=unlist(str_split(strB,"\n"))
  checkTrue(file.exists(file.path(manDir,"GenericFunc-method-#A#numeric.Rd")))
  checkTrue(file.exists(file.path(manDir,"GenericFunc-method-#B#character.Rd")))
  checkTrue(CompareTrimmedNonEmptyLines(readLines(file.path(manDir,"GenericFunc-method-#A#numeric.Rd")),strA))
  checkTrue(CompareTrimmedNonEmptyLines(readLines(file.path(manDir,"GenericFunc-method-#B#character.Rd")),refB))
}

##########################################################################
test.forGeneric=function(){
    require("stringr")
    code='  
########################################################
require("methods")
setGeneric(
  name="GenericFunc",
  def=function(# A short description of the generic function
    ### This function is generic
    .Object, ##<< d2 
    num 
    ){standardGeneric("GenericFunc")
  ##value<< d3
  }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="A",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="GenericFunc",
    signature=c("A","numeric"),
    definition=function(# A short description of the method for class A
        ### This method deals with objects of class A
       .Object, ##<< an object of class "A"
       num ##<< a number
       ){
       return(.Object@name) 
       ##value<< d3
    }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="B",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="GenericFunc",
    signature=c("B","character"),
    definition=function(# A short description of the method for class B
        ### This method deals with objects of class B
       .Object, ##<< an object of class "B"
       num 
       ){
       return(.Object@name) 
       ##value<< f3
    }
)
'
e=new.env()
old <- options(keep.source=TRUE,topLevelEnvironment=e)
## ex=function(old){
##   print('stuff')
##   print(topenv())
##   options(topLevelEnvironment=te)
##   print(topenv())
## }
#  on.exit(ex(old))
exprs <- parse(text=code,keep.source=TRUE)
    for (i in exprs){
          print(i)
          eval(i, env=e)
    }
#pe(quote(ls(e)),environment())
objs <- sapply(ls(e),get,e,simplify=FALSE) 
gens=objs[sapply(names(objs),isGeneric,e)]
#pp("gens",environment())
# now take a parser used for single objects
f_single <- extract.xxx.chunks
# show the interface of f_single
fg=objs[["GenericFunc"]]
meths=findMethods(fg,where=e)
o=meths[[1]]
src=getSource(o)
#pp("src",environment())
n=names(o)
#pe(quote(f_single(src)),environment())
resSingle=f_single(src)
checkEquals(resSingle[["item{.Object}"]],"an object of class \"A\"")
checkEquals(resSingle[["value"]],"d3")

# create an iterator 
f_funs <- forGeneric(f_single,env=e,gens=gens)
# show the interface of the iterator
res=f_funs(objs=objs,docs=list())
pp("res",environment())
checkEquals(res[["GenericFunc-method-#A#numeric"]][["item{.Object}"]],"an object of class \"A\"")
checkEquals(res[["GenericFunc-method-#B#character"]][["item{.Object}"]],"an object of class \"B\"")
checkEquals(res[["GenericFunc-method-#A#numeric"]][["value"]],"d3")
checkEquals(res[["GenericFunc-method-#B#character"]][["value"]],"f3")
}
##########################################################################
test.ParsersOnSingleFiles=function(){
require("stringr")
definition='function(# A short description of the method for class A
        ### This method deals with objects of class A
       .Object, ##<< an object of class "A"
        ### a further comment on the first argument
       num ##<< a number
       ){
       return(.Object@name) 
       ##value<< d3
       ### valuecomment
    }'
code=paste('  
########################################################
require("methods")
setGeneric(
  name="GenericFunc",
  def=function(# A short description of the generic function
    ### This function is generic
    .Object, ##<< d2 
    num 
    ){standardGeneric("GenericFunc")
  ##value<< d3
  }
)
########################################################
########################################################
setClass(# b1 
    ### b2
    Class="A",
    slots=c(name="character") # slot
   )
########################################################
setMethod(
    f="GenericFunc",
    signature=c("A","numeric"),
    definition=',
    definition,
    ')',
sep="")
e=new.env()
old <- options(keep.source=TRUE,topLevelEnvironment=e)
exprs <- parse(text=code,keep.source=TRUE)
    for (i in exprs){
          print(i)
          eval(i, env=e)
    }
objs <- sapply(ls(e),get,e,simplify=FALSE) 
gens=objs[sapply(names(objs),isGeneric,e)]
#pp("gens",environment())
# now take a parser used for single objects
fg=objs[["GenericFunc"]]
meths=findMethods(fg,where=e)
o=meths[[1]]
src=getSource(o)
res=extract.xxx.chunks(src)
ref=list()
ref[["item{.Object}"]]="an object of class \"A\""
ref[["item{num}"]]="a number"
ref[["value"]]="d3"
ref[["GenericFunc-method-#A#numeric"]]
checkEquals(ref,res)
###
res=prefixed.lines(src)
#pp("res",environment())
ref=list()
ref[["description"]]="This method deals with objects of class A"
ref[["item{.Object}"]]="a further comment on the first argument"
ref[["value"]]="valuecomment"
checkEquals(ref,res)
###
res=title.from.firstline(src)
ref=list()
ref[["title"]]="A short description of the method for class A"
checkEquals(ref,res)

###
res=definition.from.source(doc=list(definition=""),src=src) 
pp("res",environment())
checkTrue(CompareTrimmedNonEmptyLines(str_split(definition,"\n")[[1]],res$definition))
}

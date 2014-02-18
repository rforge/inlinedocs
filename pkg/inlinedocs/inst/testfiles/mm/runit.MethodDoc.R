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
	cat(file=file.path(TestDir,"example.GenericFunc_method__A_numeric.R"),text=testCodeA)
  testCodeB='
test.B(){
        reqire("RUnit")
	o=new("B",1)
	print(GenericFunc(o,"blub"))
	print(isGeneric("GenericFunc"))
} 
'
	cat(file=file.path(TestDir,"example.GenericFunc_method__B_character.R"),text=testCodeB)
	
  parsers=NULL
	result <- extract.docs.file(
    f,
    parsers,
    inlinedocs.exampleDir=TestDir,
    inlinedocs.exampleTrunk="example."
  )
  ex <- result[["GenericFunc_method__A_numeric"]]$examples
	#pp("ex",environment())
	checkTrue(CompareTrimmedNonEmptyLines(result[["GenericFunc_method__A_numeric"]]$examples,testCodeA))
	checkTrue(CompareTrimmedNonEmptyLines(result[["GenericFunc_method__B_character"]]$examples,testCodeB))
}
############################################################################################################################
test.AddExampleCodeForMethodsFromExternalTestIntoRdFile=function(){
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
pkgName='NamespaceExample'  
pkgVersion='0.0-1'  
desc <-paste("
Package:",pkgName," 
Title: Examples to test the possibilities of Namespaces  
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
	cat(file=file.path(TestDir,"example.GenericFunc_method__A_numeric.R"),text=testCodeA)
  testCodeB='
test.B(){
        reqire("RUnit")
	o=new("B",1)
	print(GenericFunc(o,"blub"))
	print(isGeneric("GenericFunc"))
} 
'
	TestDir=file.path(pkgDir,"inst","tests")
	cat(file=file.path(TestDir,"example.GenericFunc_method__B_character.R"),text=testCodeB)
	
  package.skeleton.dx(
    pkgDir,
    inlinedocs.exampleDir=TestDir,
    inlinedocs.exampleTrunk="example."
  )
  #stop("the examples do not show up in the Rd file although they are extracted")
}
############################################################################################################################
test.MethodDescription=function(){
  require("stringr")
	pkgDir="pkg"
	TestDir=file.path(pkgDir,"inst","tests")
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
	result <- extract.docs.file(
    fn,
    parsers,
    inlinedocs.exampleDir=TestDir,
    inlinedocs.exampleTrunk="example."
  )
	
  
	#pp("result",environment())

	checkEquals(result[["GenericFunc_method__A_numeric"]][["description"]],"This method deals with objects of class A")
	checkEquals(result[["GenericFunc_method__B_character"]][["description"]],"This method deals with objects of class B")
	checkEquals(result[["GenericFunc_method__A_numeric"]][["description"]],"This method deals with objects of class A")
	checkEquals(result[["GenericFunc_method__B_character"]][["description"]],"This method deals with objects of class B")
	checkEquals(result[["GenericFunc_method__A_numeric"]][["title"]],"A short description of the method for class A")
	checkEquals(result[["GenericFunc_method__B_character"]][["title"]],"A short description of the method for class B")
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
    definition=function # constructor
    (.Object,name){
      .Object@name <- name
      return(.Object)
    }
)
########################################################
setMethod(
    f="GenericFunc",
    signature=c("A","numeric"),
    definition=function(# A short description of the method for class A
        ##description<<   This method deals with objects of class A
                        
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
    definition=function # a constructor for Class B
    ### This constructor allows the creation of objects of class B 
    (
    .Object, ##<< an object of class B
    name, ##<< an string
    value ##<< a number
    ){
      .Object@name <- name
      return(.Object)
      ### The desired object of class B
    }
)
#########################################################
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
  ## create a NAMESPACE FIle since one has to be present.
namesp <- '
exportGenerics(
       GenericFunc
)
'
  NamespaceFilePath=file.path(pkgDir,"NAMESPACE")
	cat(file=NamespaceFilePath,text=namesp)
	parsers=NULL
  manDir=file.path(pkgDir,"man")
  pp("manDir",environment())
  unlink(paste(manDir,"/*",sep=""),recursive=TRUE)
  tryCatch(package.skeleton.dx(pkgDir))
  strA <- '
\\name{GenericFunc_method__A_numeric}
\\alias{GenericFunc_method__A_numeric}
\\title{A short description of the method for class A}

\\description{This method deals with objects of class A}
\\arguments{
  \\item{.Object}{an object of class "A"}
  \\item{num}{
}
}
\\author{Markus Müller}
'
  iniB <- '
\\name{initialize_method__B}
\\alias{initialize_method__B}
\\title{a constructor for Class B}
\\description{This constructor allows the creation of objects of class B }
\\arguments{
  \\item{.Object}{an object of class B}
  \\item{name}{an string}
  \\item{value}{a number}
}                                                                         
                                                                          
\\value{The desired object of class B}
                                                                          
\\author{Markus Müller}
'
  
  strB <- '
\\name{GenericFunc_method__B_character}
\\alias{GenericFunc_method__B_character}
\\title{A short description of the method for class B}
\\description{This method deals with objects of class B}
\\arguments{
  \\item{.Object}{an object of class "B"}
  \\item{num}{
}
}

\\author{Markus Müller}
  '
  strZ <- '\\name{GenericFunc-methods}
\\docType{methods}
\\alias{GenericFunc-methods}
\\alias{GenericFunc,A,numeric-method}
\\alias{GenericFunc,B,character-method}
\\title{ ~~ Methods for Function \\code{GenericFunc}  ~~}
\\description{
 ~~ Methods for function \\code{GenericFunc} ~~
}
\\section{Methods}{
  \\describe{
    \\item{\\code{signature(.Object = \"A\", num = \"numeric\")}}{
      \\code{\\link{GenericFunc_method__A_numeric}}   
    }
    \\item{\\code{signature(.Object = \"B\", num = \"character\")}}{
      \\code{\\link{GenericFunc_method__B_character}}   
    }

  }
}
\\keyword{methods}
\\keyword{ ~~ other possible keyword(s) ~~ } '

  zpath     <- file.path(manDir,"GenericFunc-methods.Rd")
  APath     <- file.path(manDir,"GenericFunc_method__A_numeric.Rd")
  BPath     <- file.path(manDir,"GenericFunc_method__B_character.Rd")
  iniAPath  <- file.path(manDir,"initialize_method__A.Rd")
  iniBPath  <- file.path(manDir,"initialize_method__B.Rd")
  #refB=unlist(str_split(strB,"\n"))
  checkTrue(file.exists(APath))
  checkTrue(file.exists(BPath))
  checkTrue(file.exists(zpath))
  checkTrue(file.exists(iniAPath))
  checkTrue(file.exists(iniBPath))
  #pe(quote(readLines(file.path(manDir,"GenericFunc_method__A_numeric.Rd"))),environment())
  #pe(quote(readLines(file.path(manDir,"initialize_method__B.Rd"))),environment())
  checkTrue(CompareTrimmedNonEmptyLines(readLines(APath),strA))
  checkTrue(CompareTrimmedNonEmptyLines(readLines(BPath),strB))
  checkTrue(CompareTrimmedNonEmptyLines(readLines(iniBPath),iniB))
  if(!CompareTrimmedNonEmptyLines(readLines(zpath),strZ)){
    refZpath=paste(zpath,".Ref",sep="")
    unlink(refZpath)
    cat(file=refZpath,strZ)
    system(paste("gvimdiff",refZpath,zpath,"&"))
    stop()
  }
}

##########################################################################
test.ParsersOnSingleMethods=function(){
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
  l=createObjects(code)# note that ls will not find S4 classes nor methods for generic functions
  objs=l[["objs"]] 
  e=l[["env"]] 
  exprs=l[["exprs"]] 
  gens=objs[sapply(names(objs),isGeneric,e)]
  #pp("gens",environment())
  # now take a parser used for single objects
  fg=objs[["GenericFunc"]]
  meths=findMethods(fg,where=e)
  o=meths[[1]]
  src=getSource(o)
  pe(quote(class(src)),environment())
  res.extract.xxx.chunks=extract.xxx.chunks(src)
  ref.extract.xxx.chunks=list()
  ref.extract.xxx.chunks[["item{.Object}"]]="an object of class \"A\""
  ref.extract.xxx.chunks[["item{num}"]]="a number"
  ref.extract.xxx.chunks[["value"]]="d3"
  ref.extract.xxx.chunks[["GenericFunc_method__A_numeric"]]
  checkEquals(ref.extract.xxx.chunks,res.extract.xxx.chunks)
  ###
  res.prefixed.lines=prefixed.lines(src)
  #pp("res.prefixed.lines",environment())
  ref.prefixed.lines=list()
  ref.prefixed.lines[["description"]]="This method deals with objects of class A"
  ref.prefixed.lines[["item{.Object}"]]="a further comment on the first argument"
  ref.prefixed.lines[["value"]]="valuecomment"
  checkEquals(ref.prefixed.lines,res.prefixed.lines)
  ###
  res.title.from.firstline=title.from.firstline(src)
  ref.title.from.firstline=list()
  ref.title.from.firstline[["title"]]="A short description of the method for class A"
  checkEquals(ref.title.from.firstline,res.title.from.firstline)
  
  ###
  res.definition.from.source=definition.from.source(doc=list(definition=""),src=src) 
  pp("res.definition.from.source",environment())
  checkTrue(CompareTrimmedNonEmptyLines(str_split(definition,"\n")[[1]],res.definition.from.source$definition))
  ### now we use another parser
  ## first we create the doc links and find the ones created by setMethod
  l= extract.file.parse(code,e)
  doc.link=l[["GenericFunc_method__A_numeric"]]
  src=getMethodSrc(doc.link,e)
  pp("src",environment())
  pe(quote(class(src)),environment())
  res.extract.xxx.chunks=extract.xxx.chunks(src)
  checkEquals(ref.extract.xxx.chunks,res.extract.xxx.chunks)

  res.prefixed.lines=prefixed.lines(src)
  checkEquals(ref.prefixed.lines,res.prefixed.lines)
  
  res.title.from.firstline=title.from.firstline(src)
  checkEquals(ref.title.from.firstline,res.title.from.firstline)
  
  res.definition.from.source=definition.from.source(doc=list(definition=""),src=src) 
  pp("res.definition.from.source",environment())
  checkTrue(CompareTrimmedNonEmptyLines(str_split(definition,"\n")[[1]],res.definition.from.source$definition))

#res=extract.docs.setMethod(MethodDocLink,e)

}

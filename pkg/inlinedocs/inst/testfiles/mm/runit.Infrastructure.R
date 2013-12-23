#
# vim:set ff=unix expandtab ts=2 sw=2:
test=function(){    
    # forfun is a factory that creates an iterator wrapper for
    # any function that operates on a single generic function 
    # (The actual parameterlist can differ for instance the src of the function or the function object itself)
    # The test shows how a function: "f_single"
    # that operates on a singe function.
    # is transformed by "forfun" into an interator 
    # function "f_generic" that takes a list of objects 
    # iterates over those that are generic Functions 
    # and ignores everything else.
    # Inlinedocs forfun has a similar purpose.
    # We make sure that forfun can be used in the same infrastructure
    # as forfun
    
    
    # first we create two objects 
    # a normal function "f"
    # and a S4 generic "GenericFunc"
    require("stringr")
    code='  
    f=function(# A short description of the non generic function
               ### This function is non generic
               x ##<< d2
               ){
               ##value<< d3
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
    '
    e=new.env()
    old <- options(keep.source=TRUE,topLevelEnvironment=e)
    exprs <- parse(text=code,keep.source=TRUE)
    for (i in exprs){
          print(i)
          eval(i, env=e)
    }
    pe(quote(ls(e)),environment())
    objs <- sapply(ls(e),get,e,simplify=FALSE) 
    gens=objs[sapply(names(objs),isGeneric,e)]
    pp("gens",environment())

    # now take a parser used for single objects
    f_single <- extract.xxx.chunks
    # show the interface of f_single
    o=objs[["GenericFunc"]]
    o=objs[["f"]]
    at=attributes(o)
    o=eval(getGeneric(objs[["GenericFunc"]]),env=e)
    at=attributes(o)
    src=str_split(attr(o,"srcref"),"\n")
    src=getSource(o)
    pp("src",environment())
    n=names(o)
    pe(quote(f_single(src,n)),environment())
    # create an iterator 
    f_funs <- forfun(f_single)
    # show the interface of the iterator
    res=f_funs(objs=objs,docs=list())
    pp("res",environment())
    sublist=list()
    sublist[["item{.Object}"]] <- "d2"
    sublist[["value"]] <- "d3"
    ref=list()
    ref[["GenericFunc"]] <- sublist
    sublist=list()
    sublist[["item{x}"]] <- "d2"
    sublist[["value"]] <- "d3"
    ref[["f"]] <- sublist
    checkEquals(ref,res)
}
############################################################################################################################
test.extract.file.parse.ClassParents=function(){
    require("stringr")
    code='  
#################################################
setClass(#ExposedClass
   ### The class description
         ##details<< Put what you like in documentation details,
         ## but ideally reference construction methods.

   Class="ExposedClass",
   representation=representation(
        times="numeric" ##<< a dummy 
   )
)
#################################################
setMethod(
   f="initialize",
   signature="ExposedClass",
   definition=function(.Object,times){
	   .Object@times <- times
	   .Object
   }
)
'
  l=createObjects(code)
  objs=l[["objs"]] 
  e=l[["env"]] 
  l= extract.file.parse(code,e)
  ## only the following objects are documented with a doclink object
  checkEquals(names(l),c("ExposedClass","initialize-method-#ExposedClass"))
  ## At the moment class doc links have no parents
  Parent<- l$ExposedClass@parent
  pp("Parent",environment())
  checkTrue(is.na(Parent))
}
############################################################################################################################
test.extract.file.parse.MethodParents=function(){
  ### the aim is to check if doc link objects for methods
  ### are created and labeled correctly (including the link to a function)
  require("stringr")
  code='  
#################################################
setClass(#ExposedClass
   ### The class description
         ##details<< Put what you like in documentation details,
         ## but ideally reference construction methods.

   Class="ExposedClass",
   representation=representation(
        times="numeric" ##<< a dummy 
   )
)
#################################################
# overload the $ operator

#define a hidden helper function (not in the NAMESPACE file   
  getSingleCol=function(x,name){
      res=""
      if(name=="times"){ res=exposedGeneric(x)}
      return(res)
  }
setMethod("$",
        def=getSingleCol,
        sig=signature("ExposedClass")
        #since $ is a already defined generic the names of the arguments are not arbitrary 
)
#################################################
setGeneric(
    name="exposedGeneric",
    def=function( # do something with several kinds of arguments
    ### Look at the methods to see which arguments are supported
    object ##<< an object that contains data and a format description.  
    ){
        standardGeneric("exposedGeneric")
    }
)
#################################################
getTime= function(object){
       return(object@times)
     }
#################################################
setMethod(
   f= "exposedGeneric",
   signature="ExposedClass",
   definition=getTime
)
#################################################
'
  l=createObjects(code)
  objs=l[["objs"]] 
  e=l[["env"]] 
  l= extract.file.parse(code,e)
 
  pp("l",environment())
  ## only the following objects are documented with a doclink object
  checkEquals(names(l),c("ExposedClass", "$-method-#ExposedClass", "exposedGeneric-method-#ExposedClass"))
  ## check that the parents are found
  MethodParent<- l[["exposedGeneric-method-#ExposedClass"]]@parent
  checkEquals(MethodParent,"getTime")
  OperatorParent<- l[["$-method-#ExposedClass"]]@parent
  checkEquals(OperatorParent,"getSingleCol")
}
############################################################################################################################
test.inherit.docs=function(){
    require("stringr")
    code='  
#################################################
setClass(#ExposedClass
   ### The class description
         ##details<< Put what you like in documentation details,
         ## but ideally reference construction methods.

   Class="ExposedClass",
   representation=representation(
        times="numeric" ##<< a dummy 
   )
)
#################################################
# overload the $ operator

#define a hidden helper function (not in the NAMESPACE file   
  getSingleCol=function(# a short headline für getSingleCol
     ### a long headline for getSingleCol			
	x,name){
      res=""
      if(name=="times"){ res=exposedGeneric(x)}
      return(res)
  }
setMethod("$",signature("ExposedClass"), #since $ is a already defined generic the names of the arguments are not arbitrary 
        definition=getSingleCol
)
#################################################
setGeneric(
    name="exposedGeneric",
    def=function( # do something with several kinds of arguments
    ### Look at the methods to see which arguments are supported
    object ##<< an object that contains data and a format description.  
    ){
        standardGeneric("exposedGeneric")
    }
)
#################################################
getTime= function(# a short headline 
     ### extract the times slot
     object ##<< an object
     ){
       return(object@times)
     }
#################################################

setMethod( 
   f= "exposedGeneric",
   signature="ExposedClass",
   definition=getTime
)
#################################################
'
  li=createObjects(code)
  objs=li[["objs"]] 
  e=li[["env"]] 
  l= extract.file.parse(code,e)
  pp("l",environment())
  
  doc.names=names(objs)
  pp("doc.names",environment())
  ## create a first result item with at least a definition field 
  name="exposedGeneric-method-#ExposedClass"
  singleRes=extract.docs(parsed=l,objs=objs,on=name)
  pp("singleRes",environment())
  checkEquals(names(singleRes),"definition")
  #### create documentation for the functions.
  funkparsers=sapply(forfun.parsers,forfun)
  docs=list()
   for(i in seq_along(funkparsers)){
     p <- funkparsers[[i]]
     N <- names(p)
     if(is.character(N) && N!=""){
         cat(" this is parser:",N,"\n",sep="")
     }else cat('.\n')
     ## This is the argument list that each parser receives:
     L <- p(code=code,objs=objs,docs=docs,env=e)
     docs <- combine(docs,L) 
   }
   ## post-process to collapse all character vectors
   for(i in seq_along(docs)){
     for(j in seq_along(docs[[i]])){
       if(names(docs[[i]])[j]!=".s3method")
       docs[[i]][[j]] <- paste(docs[[i]][[j]],collapse="\n")
     }
  }
 pp("docs",environment())
 # now let our sparse method documentation inherit the rich docu of the 
 # the functions which are its parents
 singleResNew <- inherit.docs(parsed=l,res=docs,childName=name)
 pp("singleResNew",environment())
}

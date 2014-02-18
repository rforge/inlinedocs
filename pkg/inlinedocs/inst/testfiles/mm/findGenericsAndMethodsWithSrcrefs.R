# Minimum code exampe to test method documentation
# vim:set ff=unix expandtab ts=2 sw=2
# set up some example code
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
       num
       ){
       return(.Object@name) 
    }
)
########################################################
# note that initialize is allready defined
# so we would not find a generic with srcref 
setMethod(
    f="initialize",
    signature="A",
    definition=function(.Object,name){
      .Object@name <- name
      return(.Object)
    }
)
# now find all Generics whose src can be found
GenHasSrc=function(genName){!is.null(getSrcref(getGeneric(genName)))}
GenericsWithSrcRef=getGenerics()[sapply(getGenerics(),GenHasSrc)]

# we now want to find all Generics that have at least one Method where we can get at the source
methSrc=function(MethodDefinition){getSrcref(unRematchDefinition(MethodDefinition))}
MethodHasSrc=function(MethodDefinition){!is.null(methSrc(MethodDefinition))}
MethodsWithSrcRefForGen=function(genName){ findMethods(genName)[sapply(findMethods(genName),MethodHasSrc)]}

GenHasAnyMethodWithSrc=function(genName){any(sapply(findMethods(genName),MethodHasSrc))}
# now find out which generics have any documentable methods
GensWithDocMethods=getGenerics()[sapply(getGenerics(),GenHasAnyMethodWithSrc)]
# now we can make a list of list
# containing the Methods we want to documents ordered after the name of there Generics
documentableMeths=list()
for (genName in GensWithDocMethods){
	documentableMeths[[genName]]<-MethodsWithSrcRefForGen(genName)
}






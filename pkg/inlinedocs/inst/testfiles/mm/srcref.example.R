#
# vim:set ff=unix expandtab ts=2 sw=2:
## The purpose of this session is to show that initialize methods can not be made to 
## reveal their defining source code while this works well for other S4 methods.
require("methods")
setGeneric( "myGen",function(arg){standardGeneric("myGen")})
setMethod(
  f="myGen",
  signature="numeric",
  definition=function # a function with comments in its source
    ### that are used to document it with inlinedocs
    (arg ##<< another special comment for the argument
    ){
      2*arg
      ### a descrition for the return value
    }
)
## we can get the whole function definition with comments back
## by the following snippet:
attr(getMethod("myGen","numeric"),"srcref")

## this also works for operators
setMethod("$",
    signature(x = "MyClass"),
    function 
    (x,   ##<< first arg
     name ##<< second ag
     ) { }
)
attr(getMethod("$","MyClass"),"srcref")

setClass( 
   Class="MyClass", 
   representation( 
   val="numeric" 
   ) 
)
## or other functions allready defined
setGeneric("plot")
setMethod("plot",
    signature(x = "MyClass"),
    function # a comment 
    (x, y, ...) 
    {
        stop("need a definition for the method here")
    }
)
attr(getMethod("plot","MyClass"),"srcref")

## However if we overload initialize
## there is no "srcref" attribute
## e.g.
##
setMethod(
    f="initialize",
    signature="MyClass",
    definition=function # here are also comments but we can not retrieve them
    (.Object,value){
      .Object@val<- value
      return(.Object)
    }
)

attr(getMethod("initialize","MyClass"),"srcref")



#
# vim:set ff=unix expandtab ts=2 sw=2:
##########################################################################
test.completeMethodTitleByComment=function(){
  ## the title is taken from the first comment preceding the 
  ## argument list of the function in the method definition
  code='
setGeneric(
    name="AbsoluteFractionModern_from_Delta14C",
    def=function( # convert its argument to an Absolute Fraction Modern representation
    ### The function returns an object of the same type as its imput
    ### this can be a number a matrix 
    delta14C){
        standardGeneric("AbsoluteFractionModern_from_Delta14C")
    }
)
setMethod(
   f= "AbsoluteFractionModern_from_Delta14C",
      signature("matrix"),
      definition=function #a short title
      (
      ### This method produces Delta14C values from  an Absolute Fraction Normal Matrix
	delta14C ##<< a Matrix containing the values in Delta14C format
	){
	### convert a number matrix of vector containing Delta14C values to the appropriate Absolute Fraction Modern values .
	fprime=matrix(
	    nrow=nrow(delta14C),
	    ncol=ncol(delta14C),
	    sapply(delta14C,AbsoluteFractionModern_from_Delta14C)
	)
	return(fprime)
	}
)
'
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["AbsoluteFractionModern_from_Delta14C_method__matrix"]]
  pp("result",environment())
  checkEquals(result$title,"a short title")
  #  the title can also occur after the opening "(" of the argument list 
  code='
setGeneric(
    name="AbsoluteFractionModern_from_Delta14C",
    def=function( # convert its argument to an Absolute Fraction Modern representation
    ### The function returns an object of the same type as its imput
    ### this can be a number a matrix 
    delta14C){
        standardGeneric("AbsoluteFractionModern_from_Delta14C")
    }
)
setMethod(
   f= "AbsoluteFractionModern_from_Delta14C",
      signature("matrix"),
      definition=function(#a short title
      ### This method produces Delta14C values from  an Absolute Fraction Normal Matrix
	delta14C ##<< a Matrix containing the values in Delta14C format
	){
	### convert a number matrix of vector containing Delta14C values to the appropriate Absolute Fraction Modern values .
	fprime=matrix(
	    nrow=nrow(delta14C),
	    ncol=ncol(delta14C),
	    sapply(delta14C,AbsoluteFractionModern_from_Delta14C)
	)
	return(fprime)
	}
)
'
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["AbsoluteFractionModern_from_Delta14C_method__matrix"]]
  pp("result",environment())
  checkEquals(result$title,"a short title")
}
##########################################################################
test.completeMethodDocArgList=function(){
  code='
setGeneric(
    name="AbsoluteFractionModern_from_Delta14C",
    def=function( # convert its argument to an Absolute Fraction Modern representation
    ### The function returns an object of the same type as its imput
    ### this can be a number a matrix 
    delta14C){
        standardGeneric("AbsoluteFractionModern_from_Delta14C")
    }
)
setMethod(
   f= "AbsoluteFractionModern_from_Delta14C",
      signature("matrix"),
      definition=function(#a short title
	### convert a number matrix of vector containing Delta14C values to the appropriate Absolute Fraction Modern values .
	delta14C ##<< a Matrix containing the values in Delta14C format
	){
	fprime=matrix(
	    nrow=nrow(delta14C),
	    ncol=ncol(delta14C),
	    sapply(delta14C,AbsoluteFractionModern_from_Delta14C)
	)
	return(fprime)
	}
)
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["AbsoluteFractionModern_from_Delta14C_method__matrix"]]
  #pp("result",environment())
  # look for items in the result and make sure that only one is found
  # since this has been a problem with method documentation
  matches  <- grep("item\\.*",names(result))
  nl <- length(matches)
  #pp("nl",environment())
  checkEquals(nl,1)
  checkEquals(result[["item{delta14C}"]],"a Matrix containing the values in Delta14C format")
  l=createObjects(code)# note that ls will not find S4 classes nor methods for generic functions
  objs=l[["objs"]] 
  e=l[["env"]] 
  exprs=l[["exprs"]] 
  #doclinks=extract.file.parse(code,e)
  #pp("doclinks",environment())

  #docs=extra.method.docs(code,objs,e)
  #pp("docs",environment())

}
##########################################################################
test.completeMethodDescriptionByCommentAndTag <- function(){
  code='
setGeneric(
    name="AbsoluteFractionModern_from_Delta14C",
    def=function( # convert its argument to an Absolute Fraction Modern representation
    ### The function returns an object of the same type as its imput
    ### this can be a number a matrix 
    delta14C){
        standardGeneric("AbsoluteFractionModern_from_Delta14C")
    }
)
setMethod(
   f= "AbsoluteFractionModern_from_Delta14C",
      signature("matrix"),
      definition=function #a short title
	### a description in a comment
( 
	### a second description in a comment that will be ignored if there is a first one
	delta14C ##<< a Matrix containing the values in Delta14C format
  ##description<< 1. tag
	)
   ##description<< 2. tag
  {
   ##description<< 3. tag
	fprime=matrix(
	    nrow=nrow(delta14C),
	    ncol=ncol(delta14C),
	    sapply(delta14C,AbsoluteFractionModern_from_Delta14C)
	)
	return(fprime)
	}
)
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["AbsoluteFractionModern_from_Delta14C_method__matrix"]]
  pp("result",environment())
  checkEquals(result[["description"]],"a description in a comment\n1. tag\n2. tag\n3. tag")
}


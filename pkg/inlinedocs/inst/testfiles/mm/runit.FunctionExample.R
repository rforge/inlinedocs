#
# vim:set ff=unix expandtab ts=2 sw=2:
##########################################################################
test.completeFunctionDocTitleByPrecedingComment=function(){
  ## the title is taken from the first comment preceding the 
  ## argument list of the function
  code='
  silly.example <- function # a short title
  ### a despription
  (){
}
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["silly.example"]]
  checkEquals(result$title,"a short title")
  # the title can also occur after the opening "(" of the argument list
  code='
  silly.example <- function (# a short title
  ### a despription
  ){
}
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["silly.example"]]
  checkEquals(result$title,"a short title")
}
##########################################################################
test.completeFunctionDocTitleByPrecedingCommentAndTags=function(){
  ## the title is assembled from different places in and outside the function 
  ## body
  ## Note that the preceeding comment appears last in the result
  code='
  silly.example <- function # a short title
  ##title<< a first title expansion
  (
  ##title<< a second title expansion
  )
  ##title<< a third title expansion
  {

  ##title<< a forth title expansion
  }
  ##title<< a forth title expansion
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["silly.example"]]
  pp("result",environment())
  checkEquals(result$title,"a first title expansion a second title expansion a third title expansion a forth title expansion\na short title")
}
##########################################################################
test.completeFunctionDocTitleByFunctionName=function(){
  ## the title is created from the name of the function 
  code='
  silly.example <- function (){ }
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["silly.example"]]
  pp("result",environment())
  checkEquals(result$title,"silly example")
}
##########################################################################
test.completeFunctionDocDescriptionByComment=function(){
  ## the title is taken from the first comment preceding the 
  ## argument list of the function
  code='
  silly.example <- function # a short title
  ### a description in a comment
  (){
}
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["silly.example"]]
  pp("result",environment())
  checkEquals(result$description,"a description in a comment")
}

##########################################################################
test.completeFunctionDocDescriptionByCommentAndTags=function(){
  ## the desrcipiton is taken from the first ### comment preceding the 
  ## argument list of the function and several tags
  code='
  silly.example <- function # a short title
  ### a description in a comment
  (
   ##description<< 1. tag
  )

   ##description<< 2. tag
  {
   ##description<< 3. tag
  }
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["silly.example"]]
  pp("result",environment())
  checkEquals(result$description,"a description in a comment\n1. tag\n2. tag\n3. tag")
}
##########################################################################
test.completeFunctionDocArgList=function(){
  code='
      definition=function(#a short title
      ### This method produces Delta14C values from  an Absolute Fraction Normal Matrix
	delta14C ##<< a Matrix containing the values in Delta14C format
	){
	### convert a number matrix of vector containing Delta14C values to the appropriate Absolute Fraction Modern values .
}
  '
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
	result <- extract.docs.file(fn,parsers)[["definition"]]
  # look for items in the result and mak sure that only one is found
  # since this has been a problem with method documentation
  matches  <- grep("item\\.*",names(result))
  nl <- length(matches)
  checkEquals(nl,1)
  pp("nl",environment())
  checkEquals(result[["item{delta14C}"]],"a Matrix containing the values in Delta14C format")
}


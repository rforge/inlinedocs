\name{inlinedocExample<-}
\alias{inlinedocExample<-}
\title{inlinedocExample<-}
\description{Attach example code to attribute 'ex'.}
\usage{
inlinedocExample(f) <- value
}
\arguments{
  \item{f}{the function, to which to attach code for section 'examples' in
documentation}
  \item{value}{the example code, usually the body of a function (see example)}
}



\author{Inlinedocs development team <inlinedocs-support@lists.r-forge.r-project.org>}




\examples{
### Simple Hello-World function.
helloWorld <- function(){ cat('Hello World!\n')}
inlinedocExample(helloWorld) <- function(){
	# all text including comments inside this block 
	# will go to the examples section of 
	# function helloWorld
	helloWorld()	# prints Hello World
}	
}

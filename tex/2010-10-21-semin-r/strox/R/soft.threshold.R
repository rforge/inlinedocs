##' Apply the soft-threshold function to a vector.
##' 
##' @title Soft-thresholding
##' @param x A vector of numeric data.
##' @param lambda The largest absolute value that will be mapped to
##' zero.
##' @return The vector of observations after applying the
##' soft-thresholding.
##' @callGraphPrimitives
##' @export
##' @author Toby Dylan Hocking <toby.hocking@@inria.fr>
##' @examples
##' soft.threshhold(seq(-10,10,l=50))
soft.threshhold <- function(x,lambda=1){
  stopifnot(lambda>=0)
  ifelse(abs(x)<lambda,0,x-sign(x)*lambda)
}

##' Soft-thresholding
##' 
##' Apply the soft-threshold function to a vector.
##' @param x A vector of numeric data.
##' @param lambda The largest absolute value that will be mapped to
##' zero.
##' @return The vector of observations after applying the
##' soft-thresholding.
##' @author Toby Dylan Hocking <toby.hocking@@inria.fr>
soft.threshhold <- function(x,lambda=1){
  stopifnot(lambda>=0)
  ifelse(abs(x)<lambda,0,x-sign(x)*lambda)
}

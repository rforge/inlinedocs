## @RdocFunction soft.threshold
## @title "Soft-thresholding"
## \description{
##  Apply the soft-threshold function to a vector.
## }
## @synopsis
## \arguments{
##   \item{x}{A vector of numeric data.}
##   \item{lambda}{The largest absolute value 
##      that will be mapped to zero.}
## }
## \value{
## The vector of observations after applying the
## soft-thresholding.
## }
## @author
soft.threshhold <- function(x,lambda=1){
  stopifnot(lambda>=0)
  ifelse(abs(x)<lambda,0,x-sign(x)*lambda)
}

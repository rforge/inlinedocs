soft.threshold <- function # Soft-thresholding
### Apply the soft-threshold function to a vector.
(x,
### A vector of numeric data.
 lambda=1
### The largest absolute value that will be mapped to zero.
 ){
  stopifnot(lambda>=0)
  ifelse(abs(x)<lambda,0,x-sign(x)*lambda)
### The vector of observations after applying 
### the soft-thresholding function.
}

## attr(soft.threshold,"ex") <- function(){
##   x <- seq(-5,5,l=50)
##   y <- soft.threshold(x)
##   plot(x,y)
## }

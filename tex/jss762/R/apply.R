apply <- structure(function # Apply Functions Over Array Margins
### Returns a vector or array or list of values obtained by applying a
### function to margins of an array or matrix.
(X, ##<< an array, including a matrix.
 MARGIN,
### a vector giving the subscripts which the function will be applied
### over. E.g., for a matrix \code{1} indicates rows, \code{2}
### indicates columns, \code{c(1, 2)} indicates rows and
### columns. Where \code{X} has named dimnames, it can be a character
### vector selecting dimension names.
 FUN, 
### the function to be applied: see \sQuote{Details}. In the case of
### functions like \code{+}, \code{%*%}, etc., the function name
### must be backquoted or quoted.
 ... ##<< optional arguments to \code{FUN}.
 ){
  ##keyword<< iteration array
  
  ##details<< \code{FUN} is found by a call to \code{\link{match.fun}}
  ## and typically is either a function or a symbol (e.g. a backquoted
  ## name) or a character string specifying a function to be searched
  ## for from the environment of the call to \code{apply}.
  FUN <- match.fun(FUN)

  ##details<< If \code{X} is not an array but an object of a class
  ## with a non-null \code{\link{dim}} value (such as a data frame),
  ## \code{apply} attempts to coerce it to an array via
  ## \code{as.matrix} if it is two-dimensional (e.g., a data frame) or
  ## via \code{as.array}.
  dl <- length(dim(X))
  if(!dl) stop("dim(X) must have a positive length")
  if(is.object(X))
    X <- if(dl == 2L) as.matrix(X) else as.array(X)

### Body of function contains no inline docs and is omitted for brevity.

  return(ans)
### If each call to \code{FUN} returns a vector of length \code{n},
### then \code{apply} returns an array of dimension \code{c(n,
### dim(X)[MARGIN])} if \code{n > 1}.  If \code{n} equals \code{1},
### \code{apply} returns a vector if \code{MARGIN} has length 1 and an
### array of dimension \code{dim(X)[MARGIN]} otherwise.  If \code{n}
### is \code{0}, the result has length 0 but not necessarily the
### \sQuote{correct} dimension.
###
### If the calls to \code{FUN} return vectors of different lengths,
### \code{apply} returns a list of length \code{prod(dim(X)[MARGIN])}
### with \code{dim} set to \code{MARGIN} if this has length greater
### than one.
###
### In all cases the result is coerced by \code{\link{as.vector}} to
### one of the basic vector types before the dimensions are set, so
### that (for example) factor results will be coerced to a character
### array.
},ex=function(){
  ## Compute row and column sums for a matrix:
  x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
  dimnames(x)[[1]] <- letters[1:8]
  apply(x, 2, mean, trim = .2)
  col.sums <- apply(x, 2, sum)
  row.sums <- apply(x, 1, sum)
  rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

  stopifnot( apply(x, 2, is.vector))

  ## Sort the columns of a matrix
  apply(x, 2, sort)

  ##- function with extra args:
  cave <- function(x, c1, c2) c(mean(x[c1]), mean(x[c2]))
  apply(x,1, cave,  c1="x1", c2=c("x1","x2"))

  ma <- matrix(c(1:4, 1, 6:8), nrow = 2)
  ma
  apply(ma, 1, table)  #--> a list of length 2
  apply(ma, 1, stats::quantile)# 5 x n matrix with rownames

  stopifnot(dim(ma) == dim(apply(ma, 1:2, sum)))

  ## Example with different lengths for each call
  z <- array(1:24, dim=2:4)
  zseq <- apply(z, 1:2, function(x) seq_len(max(x)))
  zseq         ## a 2 x 3 matrix
  typeof(zseq) ## list
  dim(zseq) ## 2 3
  zseq[1,]
  apply(z, 3, function(x) seq_len(max(x)))
  ## a list without a dim attribute
})

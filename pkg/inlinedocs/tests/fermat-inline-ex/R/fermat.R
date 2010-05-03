fermat.test <- function#Test an integer for primality with Fermat's little theorem.
### Fermat's little theorem states that if \eqn{n} is a prime number
### and \eqn{a} is any positive integer less than \eqn{n}, then
### \eqn{a} raised to the \eqn{n}th power is congruent to \eqn{a\
### modulo\ n}{a modulo n}.
##references<< \url{http://en.wikipedia.org/wiki/Fermat's_little_theorem}
(n ##<< the integer to test for primality.
 ){
  a <- floor(runif(1,min=1,max=n))
  ##note<< \code{fermat.test} doesn't work for integers above
  ##approximately 15 because modulus loses precision.
  a^n %% n == a
### Whether the integer passes the Fermat test for a randomized
### \eqn{0<a<n}
}
is.pseudoprime <- function#Check an integer for pseudo-primality to an arbitrary precision.
### A number is pseudo-prime if it is probably prime, the basis of
### which is the probabalistic Fermat test; if it passes two such
### tests, the chances are better than 3 out of 4 that \eqn{n} is
### prime.
##references<< Abelson, Hal; Jerry Sussman, and Julie
##Sussman. Structure and Interpretation of Computer
##Programs. Cambridge: MIT Press, 1984.
(n, ##<< the integer to test for pseudoprimality.
 times ##<< the number of Fermat tests to perform
){
  result <- if(times==0)TRUE
  ##seealso<< \code{\link{fermat.test}}
  else if(fermat.test(n)) is.pseudoprime(n,times-1)
  else FALSE
  return(result)
### Whether the number is pseudoprime
  is.pseudoprime(13,4)
}

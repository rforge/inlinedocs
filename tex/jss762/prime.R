one.is.prime <- function(x){
  stopifnot(is.integer(x))
  stopifnot(length(x)==1)
  stopifnot(x>0)
  if(x==1)return(FALSE)
  divisors <- seq(sqrt(x))[-1]
  ! (0 %in% (x%%divisors))
}

eratosthenes.sieve <- function(n){
  stopifnot(is.integer(n))
  stopifnot(length(n)==1)
  stopifnot(n>1)
  candidates <- 2:n
  biggest <- sqrt(n)
  for(i in 2:biggest){
    not <- seq(i*i,n,by=i)
    candidates <- candidates[!candidates%in%not]
  }
  candidates
}


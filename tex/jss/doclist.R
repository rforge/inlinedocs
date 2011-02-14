
give.me.a.break <- function
### Create some line breaks.
(times=1,
### The number of line breaks.
 collapse=""
### String to paste in between.
 ){
  paste(rep("\n",times),
        collapse=collapse)
### Character vector of length 1.
}

give.me.five <- function
(times=1 ##<< the number of fives
 ){
  rep(5,times)
### a vector of fives
}


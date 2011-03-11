testfun <- function(foo){
  bar
}
testfun <- function
(foo){
  bar
}
oneline <- function(foo)bar
comments <- function # title
### some docs
### more
(arg,
 arg2
 ){
  foo
###
}

comments.and.ex <- structure  (
 function
### some docs
### more
(arg,
 arg2
 ){
  foo
###
},ex=function(){
  some.code
})

## following based on code from R.oo help(Object):
require(R.oo) # or commands will fail
setConstructorS3("Person", function # Person constructor
### How to make a person object
                 (name, ##<< name by which person known
                  age ##<< age on entry to system
                  ) {
  ##details<< This system records age on entry to the system - it would be
  ## better in most cases to record date of birth.
  if (missing(name)) name <- NA;
  if (missing(age))  age <- NA;

  ##value<< Returns an R.oo object of class Person, with fields
  extend(Object(), "Person",
    .name=name, ##<<name
    .age=age ##<<age
  )
})

setMethodS3("as.character", "Person", function
### Converts Person to character string
            (x, ##<< R.oo Object of class Person
             ...) {
  paste(x$.name, "was", as.integer(x$.age), "years old.");
})


setClass("Person",representation=list(name="character",age="numeric"))
setMethod("as.character", "Person", function
### Converts Person to character string
            (x, ##<< R.oo Object of class Person
             ...) {
  paste(x@name, "was", as.integer(x@age), "years old.");
})



## The following modifications to ESS variables allow C-c C-f to send a
##  complete inlinedocs function. The mods are based on the variables as at
##  ESS 5.10. These are file-local variables - they may also be changed
##  using ess-mode-hook or directory-local-variables.
##
## modifications to ess-function-pattern allow:
## * setConstructorS3, setMethodS3
## * inlinedocs documentation between keyword function and opening "("
## * structure combining function and examples (note that R requires opening "(" to be on same line as keyword structure)
## * ess-eval-function to be called with point anywhere after the function keyword.
##
## modifications to ess-set-function-start ensure that the sent object finishes
##   at the closing ")", when the object definition uses
##   setConstructorS3, setMethodS3 or <- structure
##
## Local Variables:
## ess-function-pattern: "\\(\\(\\(\\s\"\\[?\\[?\\(\\sw\\|\\s_\\)*\\(<-\\)?\\(\\sw\\|\\s_\\)*\\s\"\\)\\|\\(\\(^\\|[ ]\\)\\(\\sw\\|\\s_\\)+\\)\\)\\s-*\\(<-\\|=\\)\\|^set\\(As\\|Method\\|MethodS3\\|ConstructorS3\\|Generic\\|GroupMethod\\|ReplaceMethod\\)(\\s\"\\[?\\[?\\(\\sw\\|\\s_\\)*\\s\",\\(\\s-\\|\n\\)*.*\\)\\(\\(\\s-\\|\n\\)*\\s<.*\\s>\\)*\\(\\s-\\|\n\\)*\\(structure\\s-*(\\s-*\\(?:\\(?:#.*\\)?\n\\s-*\\)*\\)?function\\b\\s-*\\(?:\\(?:#.*\\)?\n\\s-*\\)*(?"
## ess-set-function-start: "\\(^set[MGARC][Ma-z]+\\(?:S3\\)?\\)\\|\\(.*<-\\s-*structure\\)\\s-*("
## End:

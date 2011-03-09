silly.example <- function
### this function does nothing in particular and does it very well
(
 ##title<<Simple function arguments
 first,         ##<< the first argument with a multi-line description
 ## which I really have to put here rather than explaining in the details.
 second=        ##<< the second argument with a list default value
 ## and descriptions of each of the elements
 ##describe<<
 list(this="that", ##<< whichness
      the="other", ##<< of the
      rhubarb="stew", ##<< why
      foo="bar"),
 ##end<<
 third ##<< an argument that does nothing
 )
{
  ##description<<why should I add to description?
  ##details<<
  ## if second is TRUE then first is returned
  if ( second ){
    ##alias<<Long silly alias
    res <- first
  } else {
    ##details<<
    ## if second is not TRUE then a list is returned
    ##describe<<The contents of the list are:
    res <- list(x=7, ##<< x coordinate
                z= ##<< z describes everything else
                ##describe<<
                list(colour="green", ##<< colour of line
                     width=2),     ##<< width of line
                ##end<<
                ## and this line should get into documentation for z
                y=10)##<< y coordinate
  }
  ##note<< a note
  ##references<< a reference
  ##seealso<< \code{\link{Silly-class}}
  ##keyword<<documentation utilities
  invisible(res)
### invisible something not unrelated to first
}

setClass("Silly", # S4 classes can be documented as well
### The Silly class does nothing much either
         ##details<< Put what you like in documentation details,
         ## but ideally reference construction methods.
         representation(forwards="function", ##<< forward operation
                        reverse="function", ##<< how to go backward
                        crashes="integer") ##<< how many crashes
         ) ##<< this comment is ignored as it is outside setClass expression

## creates "show" generic function. Documentation of this not yet supported.
setMethod("show","Silly",function(object){cat("crashed ",object@crashes," times\n")})

# following based on code from R.oo help(Object):

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

age_person <- function # make Person older
### Increment Person's age
(this,                 ##<< R.oo object of class Person
 years,                ##<< how many years to add
 ...)                  ##<< ignored here
{
  paste(this$.name, "was", as.integer(this$.age), "years old.");
}
setMethodS3("older", "Person", age_person)

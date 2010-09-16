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
                list(colour=green, ##<< colour of line
                     width=2),     ##<< width of line
                ##end<<
                ## and this line should get into documentation for z
                y=10)##<< y coordinate
  }
  ##note<< a note
  ##references<< a reference
  ##seealso<< \code{\link{silly-package}}
  ##keyword<<documentation utilities
  return(res)
### invisible something not unrelated to first
  ##examples<<
  ##These lines got to the examples section in the documentation.
  res <- silly.example(first="first",second=TRUE, third=3)
}


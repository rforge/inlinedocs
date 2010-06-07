test.file <- function
### Check an R code file with inlinedocs to see if the
### extract.docs.file parser accurately extracts all the code inside!
### The code file should contain a variable .result which is the
### documentation list that you should get when you apply
### extract.docs.file to the file. We check for identity of elements
### of elements of the list, so the order of elements should not
### matter, and thus this should be a good robust unit test.
(f
### File name of R code file with inlinedocs to parse and check.
 ){
  result <- extract.docs.file(f)
  e <- new.env()
  sys.source(f,e)
  ## these are the items to check for, in no particular order
  .result <- e$.result
  for(FUN in names(.result))for(N in names(.result[[FUN]])){
    .res <- .result[[FUN]][[N]]
    res <- result[[FUN]][[N]]
    if(is.null(res) || .res!=res){
      stop(f,":\n\n",res,"\nin ",FUN,"$",N,", expected:\n\n",.res,"\n")
    }
  }
}

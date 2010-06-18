test.file <- function
### Check an R code file with inlinedocs to see if the
### extract.docs.file parser accurately extracts all the code inside!
### The code file should contain a variable .result which is the
### documentation list that you should get when you apply
### extract.docs.file to the file. We check for identity of elements
### of elements of the list, so the order of elements should not
### matter, and thus this should be a good robust unit test.
(f,
### File name of R code file with inlinedocs to parse and check.
 verbose=TRUE
### Show output?
 ){
  result <- extract.docs.file(f)
  e <- new.env()
  sys.source(f,e)
  ## these are the items to check for, in no particular order
  .result <- e$.result
  for(FUN in names(.result)){
    if(verbose)cat(FUN,"")
    for(N in names(.result[[FUN]])){
      .res <- .result[[FUN]][[N]]
      res <- result[[FUN]][[N]]
      if(is.null(res) || .res!=res){
        stop(f,":\n\n",res,"\nin ",FUN,"$",N,", expected:\n\n",.res,"\n")
      }
    }
  }
  if(verbose)cat("\n")
}
save.test.result <- function
### For unit tests, this is an easy way of getting a text
### representation of the list result of extract.docs.file.
(f
### R code file with inlinedocs to process with extract.docs.file.
 ){
  .result <- extract.docs.file(f)
  dump(".result",control=NULL)
  lines <- readLines("dumpdata.R")
  cat(paste(lines,"\n"))
}

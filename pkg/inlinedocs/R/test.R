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
  e <- new.env()
  suppressWarnings(sys.source(f,e))
  ## these are the items to check for, in no particular order
  .result <- e$.result
  parsers <- e$.parsers
  result <- extract.docs.file(f,parsers)
  for(FUN in names(.result)){
    if(verbose)cat(FUN,"")
    fun <- result[[FUN]]
    .fun <- .result[[FUN]]
    ## first check to make sure all the stored items are there
    for(N in names(.fun)){
      .res <- .fun[[N]]
      res <- fun[[N]]
      if(is.null(res) || is.na(res) || is.na(.res) || .res!=res){
        stop(f,":\n\n",res,"\nin ",FUN,"$",N,", expected:\n\n",.res,"\n")
      }
    }
    ## now check and see if there are no additional items!
    additional <- !names(fun)%in%names(.fun)
    show <- fun[additional] ##ignore NULL extracted items
    show <- show[!sapply(show,is.null)]
    if(length(show)){
      cat("\n")
      print(show)
      stop("extracted some unexpected docs!")
    }
  }
  ## make sure there are no unexpected outer lists
  not.expected <- names(result)[!names(result)%in%names(.result)]
  if(length(not.expected)){
    print(not.expected)
    stop("extracted some unexpected documentation objects!")
  }
  ## finally make a package using this file and see if it passes
  ## without warnings
  if(!is.null(e$.dontcheck))return()
  make.package.and.check(f,parsers,verbose)
  if(verbose)cat("\n")
}

### Make a package by processing f with a standard DESCRIPTION, and
### stop if there are errors or warnings.
make.package.and.check <- function(f,parsers=default.parsers,verbose=TRUE){
  pkgname <- sub(".[rR]$","",basename(f))
  pkgdir <- file.path(tempdir(),pkgname)
  if(file.exists(pkgdir))unlink(pkgdir,recursive=TRUE)
  rdir <- file.path(pkgdir,"R")
  dir.create(rdir,recursive=TRUE)
  desc <- file.path(system.file(package="inlinedocs"),"silly","DESCRIPTION")
  file.copy(desc,pkgdir)
  file.copy(f,rdir)
  print(pkgdir)
  package.skeleton.dx(pkgdir,parsers)
  cmd <- sprintf("%s CMD check %s",file.path(R.home("bin"), "R"),pkgdir)
  if(verbose)cat(cmd,"\n")
  checkLines <- system(cmd,intern=TRUE)
  warnLines <- grep("WARNING",checkLines,value=TRUE)
  if(length(warnLines)>0){
    print(warnLines)
    stop("WARNING encountered in package check!")
  }
}

save.test.result <- function
### For unit tests, this is an easy way of getting a text
### representation of the list result of extract.docs.file.
(f
### R code file with inlinedocs to process with extract.docs.file.
 ){
  .result <- extract.docs.file(f)
  dump(".result",tmp <- tempfile(),control=NULL)
  lines <- readLines(tmp)
  cat(paste(lines,"\n"))
}

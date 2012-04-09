\name{package.skeleton.dx}
\alias{package.skeleton.dx}
\alias{inlinedocs	 }
\title{Package skeleton deluxe}
\description{Generates Rd files for a package based on R code and DESCRIPTION
metadata. After inspecting the specified R code files to find
inline documentation, it calls the standard package.skeleton
function, which creates bare Rd files. The inline documentation is
added to these Rd files and then these files are copied to
pkgdir/man, possibly overwriting the previous files there.}
\usage{package.skeleton.dx(pkgdir = "..", parsers = NULL, namespace = FALSE, 
    excludePattern = FALSE, ...)}
\arguments{
  \item{pkgdir}{Package directory where the DESCRIPTION file lives. Your code
should be in pkgdir/R. We will setwd to pkgdir/R for the duration
of the function, then switch back to where you were previously.}
  \item{parsers}{List of Parser functions, which will be applied in sequence to
extract documentation from your code. Default NULL means to first
search for a definition in the variable "parsers" in
pkgdir/R/.inlinedocs.R, if that file exists. If not, we use the
list defined in options("inlinedocs.parsers"), if that is
defined. If not, we use the package global default in the variable
default.parsers.}
  \item{namespace}{A logical indicating whether a NAMESPACE file should be generated
for this package. If \code{TRUE}, all objects whose name starts
with a letter, plus all S4 methods and classes are exported.}
  \item{excludePattern}{A regular expression matching the files that are not to be processed e.g. because 
inlinedocs can not handle them yet (like generic function definitions)}
  \item{\dots}{Parameters to pass to Parser Functions.}
}



\author{Inlinedocs development team
<inlinedocs-support@lists.r-forge.r-project.org>}




\examples{
library(inlinedocs)

owd <- setwd(tempdir())

## get the path to the silly example package that is provided with
## package inlinedocs
testPackagePath=file.path( system.file(package="inlinedocs"),"silly" )
## copy example project to the current unlocked workspace that can
## be modified
file.copy(testPackagePath,".",recursive=TRUE)

## generate documentation rd-Files for this package
package.skeleton.dx("silly")

## display source file and the generated Rd file  
file.show( c(file.path("silly","R","silly.R"),
             file.path("silly","man","silly.example.Rd") ),
          header=c("source","rd") )

## check the package to see if generated documentation passes
## without WARNINGs
cmd <- sprintf("\%s CMD check silly",file.path(R.home("bin"), "R"))
print(cmd)
checkLines <- system(cmd,intern=TRUE)
warnLines <- grep("WARNING",checkLines,value=TRUE)
if(length(warnLines)>0){
  writeLines(checkLines)
  cat("\n\nLines with WARNING:\n")
  print(warnLines)
  stop("WARNING encountered in package check!")
}

## cleanup: remove the test package from current workspace again
unlink("silly",recursive=TRUE)
setwd(owd)
}

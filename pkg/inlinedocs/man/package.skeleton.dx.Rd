\name{package.skeleton.dx}
\alias{package.skeleton.dx}
\alias{inlinedocs	 }
\title{Package skeleton deluxe}
\description{Automates more of the setup process for a new source
package. After inspecting the specified R code files to find
inline documentation, it calls the standard package.skeleton
function, which creates bare Rd files. The inline documentation is
added to these Rd files and then these files are copied to
../man. It will overwrite files in the pkgdir/man directory.}
\usage{
package.skeleton.dx(pkgdir = "..", parsers = NULL, namespace = FALSE, ...)
}
\arguments{
  \item{pkgdir}{package directory where the DESCRIPTION file lives. Your code
should be in pkgdir/R. We will setwd to pkgdir/R for the duration
of the function, then switch back to where you were previously.}
  \item{parsers}{List of Parser functions, which will be applied in sequence to
extract documentation from your code. Default NULL means to first
search for a definition in the variable "parsers" in
pkgdir/R/.inlinedocs.R, if that file exists. If not, we use the
list defined in options("inlinedocs.parsers"), if that is
defined. If not, we use the package global default in the variable
default.parsers.}
  \item{namespace}{a logical indicating whether a NAMESPACE file should be generated
for this package. If \code{TRUE}, all objects whose name starts with
a letter, plus all S4 methods and classes are exported.}
  \item{\dots}{Parameters to pass to Parser functions.}
}



\author{Inlinedocs development team <inlinedocs-support@lists.r-forge.r-project.org>}




\examples{
library(inlinedocs)

owd <- setwd(tempdir())
on.exit(setwd(owd))

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
checkLines <- system("R CMD check silly",intern=TRUE)
warnLines <- grep("WARNING",checkLines,value=TRUE)
if(length(warnLines)>0){
print(warnLines)
stop("WARNING encountered in package check!")
}

## cleanup: remove the test package from current workspace again
unlink("silly",recursive=TRUE)
}

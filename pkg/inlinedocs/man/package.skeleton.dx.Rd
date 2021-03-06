\name{package.skeleton.dx}
\alias{package.skeleton.dx}
\title{Package skeleton deluxe}
\description{##############################################################
Generates Rd files for a package based on R code and DESCRIPTION
##############################################################
metadata. After inspecting the specified R code files to find
##############################################################
inline documentation, it calls the standard package.skeleton
##############################################################
function, which creates bare Rd files. The inline documentation is
##############################################################
added to these Rd files and then these files are copied to
##############################################################
pkgdir/man, possibly overwriting the previous files there.}
\usage{package.skeleton.dx(pkgdir = "..", parsers = NULL, namespace = FALSE, 
    excludePattern = FALSE, inlinedocs.documentNamespaceOnly = FALSE, 
    inlinedocs.exampleDir = file.path(pkgdir, "..", "inst", "tests"), 
    inlinedocs.exampleTrunk = "example.", ...)}\arguments{
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
  \item{excludePattern}{A regular expression matching the files that are not to be
processed e.g. because inlinedocs can not handle them yet (like
generic function definitions)}
  \item{inlinedocs.documentNamespaceOnly}{A boolean flag indicating if documentation is only built for exported}
  \item{inlinedocs.exampleDir}{A string pointing to the location where inlinedocs should search for external examples}
  \item{inlinedocs.exampleTrunk}{A string used to identify the files containing external examples in the example directory. All file names of external examples have to start with this string}
  \item{\dots}{Parameters to pass to Parser Functions.}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}




\examples{
owd <- setwd(tempdir())

## get the path to the silly example package that is provided with
## package inlinedocs
testPackagePath <- file.path(system.file(package="inlinedocs"),"silly")
## copy example project to the current unlocked workspace that can
## be modified
file.copy(testPackagePath,".",recursive=TRUE)

## generate documentation .Rd files for this package
package.skeleton.dx("silly")

## check the package to see if generated documentation passes
## without WARNINGs.
if(interactive()){
  cmd <- sprintf("\%s CMD check --as-cran silly",file.path(R.home("bin"), "R"))
  print(cmd)
  checkLines <- system(cmd,intern=TRUE)
  warnLines <- grep("WARNING",checkLines,value=TRUE)
  if(length(warnLines)>0){
    writeLines(checkLines)
    cat("\n\nLines with WARNING:\n")
    print(warnLines)
    ## disable due to bug in R CMD check:
    ## https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14875
    ##stop("WARNING encountered in package check!")
  }
}  
## cleanup: remove the test package from current workspace again
unlink("silly",recursive=TRUE)
setwd(owd)
}

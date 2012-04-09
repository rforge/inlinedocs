\name{fun}
\alias{fun}
\title{fun}
\description{A test function with an example.}
\usage{fun()}
\author{Inlinedocs development team
<inlinedocs-support@lists.r-forge.r-project.org>}
\examples{
owd <- setwd("~")
testPackagePath <- file.path( system.file(package="minimalErrorPkg"),"silly" )
file.copy(testPackagePath,".",recursive=TRUE)
## check the package to see if it passes without WARNINGs
cmd <- sprintf("\%s CMD check --as-cran silly",file.path(R.home("bin"), "R"))
print(cmd)
checkLines <- system(cmd,intern=TRUE)
warnLines <- grep("WARNING",checkLines,value=TRUE)
if(length(warnLines)>0){
  writeLines(checkLines)
  cat("\n\nLines with WARNING:\n")
  print(warnLines)
  stop("WARNING encountered in package check!")
}
setwd(owd)
}

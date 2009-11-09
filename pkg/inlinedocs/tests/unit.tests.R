library(inlinedocs)
## will be run from the tests/ directory:
test.pkg <- function(pkg){
  package.skeleton.dx(pkg)
  Rd.files <- Sys.glob(file.path(pkg,"man","*"))
  print(lapply(Rd.files,readLines))
}
pkgs <- rownames(subset(file.info(dir()),isdir==TRUE))
suppressWarnings(lapply(pkgs,test.pkg))

library(inlinedocs)
## will be run from the tests/ directory:
test.pkg <- function(pkg){
  package.skeleton.dx(pkg)
  Rd.files <- Sys.glob(file.path(pkg,"man","*"))
  lapply(Rd.files,function(f)cat(paste(readLines(f),collapse="\n")))
}
pkgs <- rownames(subset(file.info(dir()),isdir==TRUE))
res <- suppressWarnings(lapply(pkgs,test.pkg))

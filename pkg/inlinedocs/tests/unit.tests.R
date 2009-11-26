library(inlinedocs)
## will be run from the tests/ directory:
test.pkg <- function(pkg){
  package.skeleton.dx(pkg)
  Rd.files <- Sys.glob(file.path(pkg,"man","*"))
  ## now sort these in locale-independent manner
  loc <- Sys.getlocale(category = "LC_COLLATE")
  Sys.setlocale(category="LC_COLLATE","C")
  Rd.files <- sort(Rd.files)
  Sys.setlocale(category="LC_COLLATE",loc)
  lapply(Rd.files,function(f)cat(paste(readLines(f),collapse="\n")))
}
pkgs <- rownames(subset(file.info(dir()),isdir==TRUE))
res <- suppressWarnings(lapply(pkgs,test.pkg))

#!/usr/bin/Rscript
library("methods")
library("utils")
prefix="../../R"
auto_paths=Sys.glob(paste(prefix,"*.R",sep="/"))
for (f in auto_paths){
    print(f)
    source(f,echo=FALSE)
}

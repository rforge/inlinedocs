#!/usr/bin/Rscript
source("prolog.r")

testfiles <- Sys.glob("*.R")
for(f in testfiles){
  print(f)
  test.file(f,verbose=FALSE)
}




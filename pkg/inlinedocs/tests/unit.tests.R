tdir <- system.file("testfiles",package="inlinedocs")
testfiles <- Sys.glob(file.path(tdir,"*.R"))
library(inlinedocs)
for(f in testfiles)test.file(f)

Stangle("inlinedocs.Rnw")
e <- new.env()
sys.source("inlinedocs.R",e,keep.source=TRUE)
docs <- extract.docs.file("inlinedocs.R")
for(N in names(e$.result)){
  expected <- e$.result[[N]]
  generated <- docs[[N]][names(expected)]
  m <- rbind(unlist(expected),unlist(generated))
  same <- apply(m,2,function(x)x[1]==x[2])
  if(!all(same)){
    print(m[,!same])
  }
}

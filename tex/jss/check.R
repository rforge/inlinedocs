Stangle("inlinedocs.Rnw",annotate=FALSE)
e <- new.env()
sys.source("inlinedocs.R",e,keep.source=TRUE)
docs <- extract.docs.file("inlinedocs.R")
for(N in names(e$.result)){
  expected <- e$.result[[N]]
  generated <- docs[[N]][names(expected)]
  m <- cbind(expected=unlist(expected),generated=unlist(generated))
  same <- apply(m,1,function(x)x[1]==x[2])
  if(!all(same)){
    print(N)
    print(m[!same,])
  }
}

#
# vim:set ff=unix expandtab ts=2 sw=2:
require(stringr)
namedPlot=function(lexp,env){
  #get the name of the caller
  print(sys.calls())
  fileName=paste(as.character(sys.calls()[[sys.nframe()-1]]),"pdf",sep=".")
  plotAndCheck(fileName,lexp,env)
}
#####################################################################################################
plotAndCheck=function(fileName,lexp,env){
pdf(file=fileName)
eval(lexp,env)
dev.off()
res=system(command=paste("qpdf --check ",fileName,sep=""))
checkEquals(attr(res,"status"),NULL)
}
#####################################################################################################
pp=function(# print out an 
### This function is used to print out a value of a variable together with its name and is helpful for debugging
string,env){
  cat("\n########################################################################\n")
  print(paste("pp:",string,"="))
  print(get(string,env))
}
#####################################################################################################
pe=function(string,env){
  cat("\n########################################################################\n")
print("pe:")
print(string)
print("=")
print(eval(string,env))
}
#####################################################################################################
trimmedNonEmptyLines=function(s){
  t=str_trim(unlist(str_split(s,"\n")))
  return(t[nchar(t)>0])
}
#####################################################################################################
CompareTrimmedNonEmptyLines=function(s1,s2){
  t1=trimmedNonEmptyLines(s1)
  t2=trimmedNonEmptyLines(s2)
  l1=length(t1)
  l2=length(t2)
  if (l1!=l2){
    print(paste("The number of lines differs l1=",l1,"  l2=",l2 ))
    return(FALSE)
  }
  for(i in (1:l1)){
    ti1=t1[[i]]
    ti2=t2[[i]]
    if (ti1!=ti2) {
      print("\n")
      print(ti1)
      print(ti2)
      print(paste("line",i,"does not match"))
      return(FALSE)
    }
  }
  return(TRUE)
}

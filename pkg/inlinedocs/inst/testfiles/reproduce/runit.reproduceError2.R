#
# vim:set ff=unix expandtab ts=2 sw=2:
test.ExtraMethodDocumentationFile=function(){
  subdir="MethodDoc"

    
  d="testDirs"
	parsers=NULL
  mandir=file.path("testDirs",subdir,"man")
  unlink(paste(mandir,"/*",sep=""))
  pwd=setwd(d)
  tryCatch(package.skeleton.dx("MethodDoc"),finally=setwd(pwd))
}


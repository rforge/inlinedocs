### Necessary fields in DESCRIPTION, otherwise error.
fields <- c("Package","Maintainer","Author","Version",
            "License","Title","Description")

`inlinedocExample<-` <- function (
	### Attaching example code to attribute 'ex'.
	f			##<< the function, to which to attach code for section 'examples' in documentation
	, value		##<< the example code, usually the body of a function (see example)
) {
	# see parsers.R examples.in.attr
	attr (f, "ex") <- value
	f
}
inlinedocExample(`inlinedocExample<-`) <- function(){
	### Simple Hello-World function.
	helloWorld <- function(){ cat('Hello World!\n')}
	inlinedocExample(helloWorld) <- function(){
		# all text including comments inside this block 
		# will go to the examples section of 
		# function helloWorld
		helloWorld()	# prints Hello World
	}	
}
		
### Default DESCRIPTION, written if it doesn't exist.  TODO, PhG:
### start with reasonable values here!
empty.description <- matrix("",ncol=length(fields),dimnames=list(NULL,fields))

package.skeleton.dx <- function # Package skeleton deluxe
### Automates more of the setup process for a new source
### package. After inspecting the specified R code files to find
### inline documentation, it calls the standard package.skeleton
### function, which creates bare Rd files. The inline documentation is
### added to these Rd files and then these files are copied to
### ../man. It will overwrite files in the pkgdir/man directory.
(pkgdir="..",
### package directory where the DESCRIPTION file lives. Your code
### should be in pkgdir/R. We will setwd to pkgdir/R for the duration
### of the function, then switch back to where you were previously.
 parsers=NULL,
### List of Parser functions, which will be applied in sequence to
### extract documentation from your code. Default NULL means to first
### search for a definition in the variable "parsers" in
### pkgdir/R/.inlinedocs.R, if that file exists. If not, we use the
### list defined in options("inlinedocs.parsers"), if that is
### defined. If not, we use the package global default in the variable
### default.parsers.
 # PhG: added to support NAMESPACE creation!
 namespace = FALSE,
### a logical indicating whether a NAMESPACE file should be generated
### for this package. If \code{TRUE}, all objects whose name starts with
### a letter, plus all S4 methods and classes are exported.
 ...
### Parameters to pass to Parser functions.
 ){
  ##alias<< inlinedocs	 
	 
  chdir <- file.path(pkgdir,"R")
  if(!file.exists(chdir))stop("need pkgdir/R, tried ",chdir)
  old.wd <- setwd(chdir)
  on.exit(setwd(old.wd))
  # PhG: R allows for specific code to be in /unix, or /windows subdirectories
  # but apparently, inlinedocs does not support this. I think it is fair to
  # stop here with an explicit error message if at least one of /unix or
  # /windows subdirectory is found!
  # file_test(-d, ...) does the job, but I don't want to add a dependency on
  # package 'utils", where it lives. So, I prefer using file.info()
  if (isTRUE(file.info("unix")$isdir) || isTRUE(file.info("windows")$isdir))
    stop("Platform-specific code in ./R/unix, or ./R/windows is not supported")
  
  ## if no DESCRIPTION, make one and exit.
  descfile <- file.path("..","DESCRIPTION")
  if(!file.exists(descfile)){
    write.dcf(empty.description,descfile)
    stop("Need ",descfile,"; please fill that in and try again")
  }

  ## Read description and check for errors
  desc <- read.dcf(descfile)
  if(any(f <- !sapply(fields,is.element,colnames(desc))))
    stop("Need ", paste(names(f)[f], collapse = ", "), " in ", descfile)
    #PhG: corrected from stop("Need ",names(f)[f]," in ",descfile)
  if(any(f <- sapply(fields,function(f)desc[,f]=="")))
    stop("Need a value for ", paste(names(f)[f], collapse = ", "),
         " in ", descfile)
    #PhG: corrected from stop("Need a value for ",names(f)[f]," in ",descfile)

  ## Load necessary packages before loading pkg code
  if("Depends" %in% colnames(desc)){
    required <- strsplit(desc[,"Depends"],split=",")[[1]]
    # PhG: for packages with NAMESPACE, dependencies are also listes in the
    # Imports field!
  } else required <- character(0)
  # PhG: packages listed in Imports field are not supposed to be attached to the
  # search path when a package with NAMESPACE is loaded, only the namespace is
  # loaded. However, the code here is not loaded as it should be, and we need to
  # load also these Import(ed) packages to get correct results in the present
  # case (to be checked with most complex cases!)
  if ("Imports" %in% colnames(desc))
    required <- c(required, strsplit(desc[, "Imports"], split = ",")[[1]])
  ## This may need some refining, basically I just tried to take the
  ## first word from each vector element, stripping of whitespace in
  ## front and anything after:
  #pkgnames <- gsub("\\W*(\\w+)\\b.*","\\1",required)
  # PhG: the previous line is wrong: it does not work with package names
  # like R.oo... Extract from Writing R Extensions manual:
  # "The `Package' and `Version' fields give the name and the version of the
  # package, respectively. The name should consist of letters, numbers, and the
  # dot character and start with a letter." 
  # Consequently, I propose:
  pkgnames <- gsub("\\W*([a-zA-Z][a-zA-Z0-9.]*)\\b.*", "\\1", required)
  # PhG: We need to eliminate 'R' from the list!
  pkgnames <- pkgnames[pkgnames != "R"]
  # PhG: if we create a namespace, we need to keep this list for further use
  if (isTRUE(namespace)) allpkgs <- pkgnames
  # PhG: We eliminate also from the list the packages that are already loaded
  pkgnames <- pkgnames[!sprintf("package:%s",pkgnames) %in% search()]
  # PhG: according to Writing R Extensions manual, a package name can occur
  # several times in Depends
  pkgnames <- unique(pkgnames)
  if (length(pkgnames)) {
    # PhG: A civilized function returns the system in the same state it was
    # before => detach loaded packages at the end!
    on.exit(try(for (pkg in pkgnames) detach(paste("package", pkg, sep = ":"),
        unload = TRUE, character.only = TRUE), silent = TRUE), add = TRUE)
    # PhG: Shouldn't we need to check that packages are loaded and shouldn't
    # we exit with an explicit error message if not? Note: we don't use version
    # information here. That means we may well load wrong version of the
    # packages... and that is NOT detected as an error!
    #for(pkg in pkgnames)try(library(pkg,character.only=TRUE),silent=TRUE)
    pkgmissing <- character(0)
    for (pkg in pkgnames) {
        res <- try(library(pkg, character.only = TRUE), silent = TRUE)
        if (inherits(res, "try-error"))
            pkgmissing <- c(pkgmissing, pkg)
    }
    if (length(pkgmissing))
        stop("Need missing package(s): ", paste(pkgmissing, collapse = ", "))
  }

  ## Essentially inlinedocs is this function, package.skeleton.dx,
  ## that turn R/*.R, tests/*.R, and DESCRIPTION files into Rd
  # PhG: this is tests/*.R, not test/*.R. Also, it is a bit ennoying to mix
  # examples and tests in tests/*.R... It means that your 'tests'/'examples'
  # will be run twice! Moreover, adding something to 'tests' causes a large
  # overhead in compiling R packages on Mac OS X. For packages without C or
  # FORTRAN code to compile (like those targetted by inlinedocs), it is
  # easy to compile the packages using R CMD build on a Mac without any other
  # addition... In case there is something in /tests, one has to install almost
  # 2Gb of latest version of Xtools, downloaded on Mac web site after login
  # (Gaps!). So, if one could avoid this painfull task, it would be wonderful!
  # My suggestion would be to place example code in the /ex subdirtectory of the
  # source of the package...
  ## files. This is done by parsing them and making a list called
  ## "docs" that summarizes them. This list is then used to edit the
  ## Rd files output by package.skeleton and produce the final Rd
  ## files. The old way of creating the docs list is rather
  ## monolithic, and for maintenance purposes I would like to begin
  ## modularization of this process. What is the best way to do this?
  ## I propose that package.skeleton.dx() starts docs as an empty
  ## list, then parses the concatenated R files as "objs," reads their
  ## text as "code," reads the package description as "desc," then
  ## passes these to a list of functions that will sequentially add
  ## things to docs. This will make extension of inlinedocs quite
  ## easy, since all you would need to do is write a new parser
  ## function and add it to the list.

  ## for the parser list, first try reading package-specific
  ## configuration file
  if(is.null(parsers))parsers <- tryCatch({
    cfg <- new.env()
    sys.source(cfile <- ".inlinedocs.R",cfg)
    L <- cfg$parsers
    if(!is.null(L))cat("Using parsers in ",cfile,"\n",sep="")
    L
  },error=function(e)NULL)
  ## then try the global options()
  opt <- "inlinedocs.parsers"
  if(is.null(parsers)&&!is.null(parsers <- getOption(opt))){
    cat("Using parsers in option ",opt,"\n",sep="")
  }
  ## if nothing configured, just use the pkg default
  if(is.null(parsers))parsers <- default.parsers
  
  ## concatenate code files and parse them
  # PhG: in Writing R Extensions manuals, source code in /R subdirectory can
  # have .R, .S, .q, .r, or .s extension. However, it makes sense to restrict
  # this to .R only for inlinedocs, but a clear indication is required in the
  # man page!
  code_files <- if(!"Collate"%in%colnames(desc))Sys.glob("*.R")
  else strsplit(gsub("\\s+"," ",desc[,"Collate"]),split=" ")[[1]]
  # PhG: one must consider a potential Encoding field in DESCRIPTION file!
  # which is used also for .R files according to Writing R Extensions
  if ("Encoding" %in% colnames(desc)) {
    oEnc <- options(encoding = desc[1, "Encoding"])$encoding
    on.exit(options(encoding = oEnc), add = TRUE)
  }
  code <- do.call(c,lapply(code_files,readLines))
  
  docs <- apply.parsers(code,parsers,verbose=TRUE,desc=desc)

  ## Make package skeleton and edit Rd files (eventually just don't
  ## use package.skeleton at all?)
  name <- desc[,"Package"]
  unlink(name,rec=TRUE)
  # PhG: added namespace argument to package.skeleton()
  # twutz: addedd suppressWarnings around package.skeleton, 
  # because I always got the warning that the package does not exist
  suppressWarnings(package.skeleton(name,code_files=code_files, namespace = isTRUE(namespace)))
  cat("Modifying files automatically generated by package.skeleton:\n")
  ## documentation of generics may be duplicated among source files.
  dup.names <- duplicated(names(docs))
  if ( any(dup.names) ){
    warning("duplicated file names in docs: ",paste(names(docs)[dup.names]))
  }
  for(N in unique(names(docs))) modify.Rd.file(N,name,docs)
  file.copy(file.path(name,'man'),"..",rec=TRUE)
  # PhG: copy NAMESPACE file back
  if (isTRUE(namespace)) {
    # PhG: package.skeleton() does not add import() statement, but here the
    # philosophy is to get a fully compilable package, which is not the
    # case at this stage with a NAMESPACE. So, we add all packages listed
    # in Depends and Imports fields of the DESCRIPTION file in an import()
    # statement in the NAMESPACE
    nmspFile <- file.path("..", "NAMESPACE")
    cat("import(", paste(allpkgs, collapse = ", "), ")\n\n", sep = "",
        file = nmspFile) 
    # PhG: append the content of the NAMESPACE file generated by
    # package.skeleton()
    file.append(nmspFile, file.path(name,'NAMESPACE'))
    # PhG: we also have to export S3 methods explictly in the NAMESPACE
    cat("\n", file = nmspFile, append = TRUE)
    for (N in unique(names(docs))) {
        d <- docs[[N]]
        if (!is.null(d$s3method))
            cat('S3method("', d$s3method[1], '", "', d$s3method[2], '")\n',
                sep = "", file = nmspFile, append = TRUE)
    }
  }
  
  unlink(name,rec=TRUE)
}
inlinedocExample(package.skeleton.dx) <- function(){
  library(inlinedocs)
  
  ## get the path to the silly example package that is provided with
  ## package inlinedocs
  testPackagePath=file.path( system.file(package="inlinedocs"),"silly" )
  ## copy example project to the current unlocked workspace that can
  ## be modified
  file.copy(testPackagePath,".",recursive=TRUE)
  
  ## generate documentation rd-Files for this package
  package.skeleton.dx("silly")
  
  ## display source file and the generated Rd file  
  file.show( c(file.path("silly","R","silly.R"),
               file.path("silly","man","silly.example.Rd") ),
            header=c("source","rd") )

  ## check the package to see if generated documentation passes
  ## without WARNINGs
  checkLines <- system("R CMD check silly",intern=TRUE)
  warnLines <- grep("WARNING",checkLines,value=TRUE)
  if(length(warnLines)>0){
    print(warnLines)
    stop("WARNING encountered in package check!")
  }
  
  ## cleanup: remove the test package from current workspace again
  unlink("silly",recursive=TRUE)
}




modify.Rd.file <- function
### Add inline documentation from comments to an Rd file
### automatically-generated by package.skeleton.
(N,
### Name of function/file to which we will add documentation.
 pkg,
### Package name.
 docs
### Named list of documentation in extracted comments.
 ){
  # PhG: for functions like 'obj<-', package.skeleton creates files like 'obj_-'
  # => rework names the same way, i.e., using the same function from utils package
  Nme <- utils:::.fixPackageFileNames(N)
  fb <- paste(Nme,".Rd",sep="")
  ## For some functions, such as `[[.object`, package.skeleton (as used
  ## within this package but not when used standalone) seems to generate
  ## with a preceding z ("z[[.object.Rd"), so the z form is tested for and
  ## used if it exists and the first does not.
  zfb <- paste("z",Nme,".Rd",sep="")
  f <- file.path(pkg,'man',fb)
  if ( (!file.exists(f)) && file.exists(file.path(pkg,'man',zfb)) ){
    fb <- zfb
    f <- file.path(pkg,'man',zfb)
  }
  ## If there are no significant docs in the comments then the object
  ## should still be documented, by writing the file by hand in the
  ## man directory. This will write a blank Rd file if none exists, so
  ## it's easy to get started.
  if((length(docs[[N]])<3) &&
     file.exists(file.path("..","man",fb))){
    unlink(f)
    return()
  }
  cat(N,":",sep="")
  d <- docs[[N]]
  dlines <- readLines(f)

  ## cut out alias line if we are in the package file and there is a
  ## matching function
  if(length(grep("-package$",N)))
    dlines <- dlines[-grep(paste("alias[{]",N,sep=""),dlines)-1]
  else if ( "alias" %in% names(d) ){
    ## allowing alias changes have to make sure that original alias remains
    ## note that the contents of this go inside \alias{}, so the separator
    ## has to terminate one and start the next
    d[["alias"]] <- paste(paste(N,"}\n\\alias{",sep=""),
                            d[["alias"]],sep="")
  }

  # PhG: in the special case of custom operators like %....%, we must protect
  # these strings in name, alias and usage (at least)! Otherwise, bad things
  # happen with these strings: (1) usage entry is cut out, because confused
  # with comments, and % are escaped in name and alias!
  if (grepl("^%.+%$", N)) {
    Nmask <- gsub("%", "---percent---", N)
    # Replace any occurence of N by Nmask
    dlines <- gsub(N, Nmask, dlines, fixed = TRUE)
  } else Nmask <- NULL

  ## cut out all comments {} interferes with regex matching
  comments <- grep("^[%~]",dlines)
  ## gotcha! if no comment lines, then -(nothing) kills everything
  if ( 0 < length(comments) ) dlines <- dlines[-comments]
  ## and class skeletons have a different way of using ~
  dlines <- gsub("\\s*~.*~\\s*","",dlines,perl=TRUE)
  ## and the "Objects can be created..." boilerplate also breaks perl REs
  dlines <- gsub("Objects can be created by calls.*\\)\\}","",dlines,perl=TRUE)
  ## ditto the "or \code{\linkS4class{CLASSNAME}} for links to other classes"
  dlines <- gsub("or \\\\code\\{\\\\linkS4class.*classes","",dlines,perl=TRUE)

  ## cut out a couple of sections that cause warnings
  o <- grep("Optionally",dlines)
  if(length(o))dlines <- dlines[-(o:(o+1))]
  ## delete examples til the end of the file (also includes keywords)
  dlines <- dlines[1:(tail(grep("examples[{]$",dlines),1)-1)]
  ## add back a minimal examples section to find and replace
  dlines <- c(dlines,"\\examples{}\n")
  ## and replace keyword section if keywords are present.
  if ( "keyword" %in% names(d) ){
    dlines <- c(dlines,"\\keyword{}\n")
  }

  ## erase curly braces in format section, which appear sporadically
  ## and can cause errors in R CMD check.
  fstart <- grep("^\\\\format[{]$",dlines)+1
  if(length(fstart)){
    closing <- grep("^[}]$",dlines)
    fend <- closing[closing>fstart][1]-1
    dlines[fstart:fend] <- gsub("[{}]","",dlines[fstart:fend])
  }
  ## At least in my code, any remaining % symbols are in \usage sections
  ## as function arguments. These promptly break Rd check because you end
  ## up with unterminated strings. Just in case, the following regexp only
  ## modifies those % symbols which follow something other than %.
  ## (a more complicated version would attempt to do so only within strings.)
  dlines <- gsub("([^%])%","\\1\\\\%",dlines,perl=TRUE)
  
  # PhG: now restore masked function name, if any (case of %....% operators)
  if (!is.null(Nmask))
    dlines <- gsub(Nmask, N, dlines, fixed = TRUE)
  
  ## Find and replace based on data in d
  txt <- paste(dlines,collapse="\n")
  for(torep in names(d)){
    if ( "s3method" == torep ){         # s3method is a flag handled later
      next
    }
    cat(" ",torep,sep="")
    FIND1 <- gsub("\\\\","\\\\\\\\",torep)
    FIND <- paste(gsub("([{}])","\\\\\\1",FIND1),"[{][^}]*[}]",sep="")
    ## need to escape backslashes for faithful copying of the comments
    ## to the Rd file:
    REP <- paste(FIND1,"{",gsub("\\\\","\\\\\\\\",d[[torep]]),"}",sep="")
    ## escape percent signs in R code:
    REP <- gsub("%","\\\\\\\\%",REP)
    ## alias (in particular) need to change only the first one generated
    ## (generic methods in classes add to standard skeleton alias set)
    if ( torep %in% c("alias") ){
      txt <- sub(FIND,REP,txt)
    } else {
      txt <- gsub(FIND,REP,txt)
    }
    classrep <- sub("item{(.*)}","item{\\\\code{\\1}:}",torep,perl=TRUE)
    if ( classrep != torep ){
      ## in xxx-class files, slots are documented with:
      ## \item{\code{name}:}{Object of class \code{"function"} ~~ }
      ## which requires slightly different processing
      FIND1 <- gsub("\\\\","\\\\\\\\",classrep)
      FIND <- paste(gsub("([{}])","\\\\\\1",FIND1),"\\{Object of class \\\\code\\{\\\"(\\S+)\\\"\\}[^}]*[}]",sep="")
      ## need to escape backslashes for faithful copying of the comments
      ## to the Rd file and also put the class type in parentheses.
      REP <- paste(FIND1,"{(\\\\code{\\1}) ",gsub("\\\\","\\\\\\\\",d[[torep]]),"}",sep="")
      ## escape percent signs in R code:
      REP <- gsub("%","\\\\\\\\%",REP)
      txt <- gsub(FIND,REP,txt)
    }
  }

  ## Fix usage
  m <- regexpr("usage[{][^}]*[}]",txt)
  Mend <- m+attr(m,"match.length")
  utxt <- substr(txt,m,Mend)
  if(length(grep("usage[{]data",utxt))){
     utxt <- gsub("data[(]([^)]*)[)]","\\1",utxt)
   }
  
  ## fix \method version if s3method
  if ( !is.null(d$s3method) ){
    pat <- paste(d$s3method,collapse=".")
    rep <- paste("\\method{xx",d$s3method[1],"}{",d$s3method[2],"}",sep="")
    utxt <- gsub(pat,rep,utxt,fixed=TRUE)
    
    # PhG: there is the special case of generic<-.obj(x, ..., value) to rewrite
    # \method{generic}{obj}(x, ...) <- value
    if (grepl("<-$", d$s3method[1])) {
        # 1) replace {generic<-} by {generic}
        utxt <- sub("<-[}]", "}", utxt)
        # 2) replace ..., value) by ...) <- value
        utxt <- sub(", *([^),]+)[)]", ") <- \\1", utxt)
    }
  } else {
    # PhG: in case we have fun<-(x, ..., value), we must rewrite it
    # as fun(x, ...) <- value
    if (grepl("<-$", N)) {
        utxt <- sub("<-[(](.+), ([^,)]+)[)]",
            "(\\1) <- \\2", utxt)
    }
    # PhG: this is for special functions %...% which should write x %...% y
    if (grepl("^%.*%$", N)) {
        utxt <- sub("(%.*%)[(]([^,]+), ([^)]+)[)]",
            "\\2 \\1 \\3", utxt) 
    }
  }
  ## add another backslash due to bug in package.skeleton
  ## but only if not before % character due to another bug if % in usage
  ## arguments - see above
  txt <- paste(substr(txt,1,m-1),
               gsub("\\\\([^%])","\\\\\\\\\\1",utxt),
               substr(txt,Mend+1,nchar(txt)),
               sep="")
  ## delete empty sections to suppress warnings in R CMD check
  txt <- gsub("\\\\[a-z]+[{]\\W*[}]","",txt)
  if ( !is.null(d$s3method) ){
    ## and now remove the xx inserted above to prevent \method{[[}{...} falling
    ## foul of the above replacement!
    txt <- gsub("\\\\method{xx","\\method{",txt,fixed=TRUE)
  }
  ## This doesn't work if there are quotes in the default values:
  ## gsub(",",paste("\n",paste(rep(" ",l=nchar(N)-1),collapse="")),utxt)
  cat(txt,file=f)
  cat("\n")
}

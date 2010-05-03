### combine lists or character strings
combine <- function(x,...)UseMethod("combine")
### combine character strings by pasting them together
combine.character <- function(x,y)paste(x,y,sep="\n")
### combine lists by adding elements or adding to existing elements
combine.list <- function(x,y){
  toadd <- !names(y)%in%names(x)
  toup <- names(y)[names(y)%in%names(x)]
  x[names(y)[toadd]] <- y[toadd]
  for(up in toup)x[[up]] <- combine(x[[up]],y[[up]])
  x
}

### Prefix for code comments used with grep and gsub.
prefix <- "^[ \t]*###[ \t]"	#changed the pattern to handle tabs and white spaces at the beginning

decomment <- function
### Remove comment prefix and join lines of code to form a
### documentation string.
(comments
### Character vector of prefixed comment lines.
 ){
  paste(gsub(prefix,"",comments),collapse="\n")
### String without prefixes or newlines.
}

### Necessary fields in DESCRIPTION, otherwise error.
fields <- c("Package","Maintainer","Author","Version",
            "License","Title","Description")
### Default DESCRIPTION, written if it doesn't exist.
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
 ...
### Parameters to pass to Parser functions.
 ){
  chdir <- file.path(pkgdir,"R")
  old.wd <- setwd(chdir)
  on.exit(setwd(old.wd))

  ## if no DESCRIPTION, make one and exit.
  descfile <- file.path("..","DESCRIPTION")
  if(!file.exists(descfile)){
    write.dcf(empty.description,descfile)
    stop("Need ",descfile,"; please fill that in and try again")
  }

  ## Read description and check for errors
  desc <- read.dcf(descfile)
  if(any(f <- !sapply(fields,is.element,colnames(desc))))
    stop("Need ",names(f)[f]," in ",descfile)
  if(any(f <- sapply(fields,function(f)desc[,f]=="")))
    stop("Need a value for ",names(f)[f]," in ",descfile)

  ## Load necessary packages before loading pkg code
  if("Depends" %in% colnames(desc)){
    required <- strsplit(desc[,"Depends"],split=",")[[1]]
    ## This may need some refining, basically I just tried to take the
    ## first word from each vector element, stripping of whitespace in
    ## front and anything after:
    pkgnames <- gsub("\\W*(\\w+)\\b.*","\\1",required)
    for(pkg in pkgnames)try(library(pkg,character.only=TRUE),silent=TRUE)
  }

  ## Essentially inlinedocs is this function, package.skeleton.dx,
  ## that turn R/*.R, test/*.R, and test/*.R files into Rd files. This
  ## is done by parsing them and making a list called "docs" that
  ## summarizes them. This list is then used to edit the Rd files
  ## output by package.skeleton and produce the final Rd files. The
  ## old way of creating the docs list is rather monolithic, and for
  ## maintenance purposes I would like to begin modularization of this
  ## process. What is the best way to do this? I propose that
  ## package.skeleton.dx() starts docs as an empty list, then parses
  ## the concatenated R files as "objs," reads their text as "code,"
  ## reads the package description as "desc," then passes these to a
  ## list of functions that will sequentially add things to docs. This
  ## will make extension of inlinedocs quite easy, since all you would
  ## need to do is write a new parser function and add it to the list.

  ## concatenate code files and parse them
  code_files <- Sys.glob("*.R")
  code <- do.call(c,lapply(code_files,readLines))

  ## write code to a file and parse it to r objs
  code.file <- tempfile()
  writeLines(code,code.file)
  e <- new.env()
  old <- options(keep.source.pkgs=TRUE)
  tryCatch(suppressWarnings(sys.source(code.file,e)),error=function(e){
    stop("source ",code.file," failed with error:\n",e)
  })
  options(old)
  objs <- sapply(ls(e),get,e,simplify=FALSE)

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
    cat("Using parsers in option ",opt,"\n")
  }
  ## if nothing configured, just use the pkg default
  if(is.null(parsers))parsers <- default.parsers
  
  ## apply parsers in sequence to code and objs
  docs <- list()
  for(i in seq_along(parsers)){
    N <- names(parsers[i])
    if(is.character(N) && N!=""){
      cat(N," ",sep="")
    }else cat('. ')
    p <- parsers[[i]]
    ## This is the argument list that each parser receives:
    L <- p(code=code,objs=objs,desc=desc,docs=docs,...)
    docs <- combine(docs,L)
  }
  cat("\n")

  ## Make -package Rd file
  name <- desc[,"Package"]
  in.details <- setdiff(colnames(desc),"Description")
  details <- paste(paste(in.details,": \\tab ",desc[,in.details],"\\cr",sep=""),
                   collapse="\n")
  docs[[paste(name,"-package",sep="")]] <-
    list(title=desc[,"Title"],
         description=desc[,"Description"],
         `tabular{ll}`=details,
         author=desc[,"Maintainer"])

  ## Make package skeleton and edit Rd files (eventually just don't
  ## use package.skeleton at all?)
  unlink(name,rec=TRUE)
  package.skeleton(name,code_files=code_files)
  cat("Modifying files automatically generated by package.skeleton:\n")
  ## documentation of generics may be duplicated among source files.
  dup.names <- duplicated(names(docs))
  if ( any(dup.names) ){
    warning("duplicated file names in docs: ",paste(names(docs)[dup.names]))
  }
  for(N in unique(names(docs))) modify.Rd.file(N,name,docs)
  file.copy(file.path(name,'man'),"..",rec=TRUE)
  unlink(name,rec=TRUE)
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
  fb <- paste(N,".Rd",sep="")
  ## For some functions, such as `[[.object`, package.skeleton (as used
  ## within this package but not when used standalone) seems to generate
  ## with a preceding z ("z[[.object.Rd"), so the z form is tested for and
  ## used if it exists and the first does not.
  zfb <- paste("z",N,".Rd",sep="")
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

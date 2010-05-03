### For each object in the package that satisfies the criterion
### checked by subfun, parse source using FUN and return the resulting
### documentation list.
forall <- function
(FUN,
### Function to apply to each element in the package.
 subfun=function(x)TRUE
### Function to select subsets of elements of the package, such as
### is.function. subfun(x)==TRUE means FUN will be applied to x and
### the result will be returned.
 ){
  function(objs,docs,...){
    objs <- objs[sapply(objs,subfun)]
    L <- list()
    for(N in names(docs)){
      o <- objs[[N]]
      L[[N]] <- FUN(src=attr(o,"source"),
                    name=N,objs=objs,o=o,docs=docs,doc=docs[[N]],...)
    }
    L
  }
### A Parser Function.
}

### For each function in the package, do something.
forfun <- function(FUN)forall(FUN,is.function)

### Default parsers to use with package.skeleton.dx
forall.parsers <-
  list(## Extract lots of info from normal functions.
       parsefun=list(forfun,function(src,name,...){
         extract.docs.fun(src,name)
       }),
       ## Fill in author from DESCRIPTION and titles.
       author.from.description=list(forall,function(desc,...){
         list(author=desc[,"Maintainer"])
       }),
       ## The format section sometimes causes problems, so erase it.
       erase.format=list(forall,function(...){
         list(format="")
       }),
       ## Convert the function name to a title.
       title.from.name=list(forall,function(name,doc,...){
         if("title"%in%names(doc))list() else
         list(title=gsub("[._]"," ",name))
       }),
       ## Get examples for FUN from the file test/FUN.R
       examples.from.testfile=list(forfun,function(name,...){
         tfile <- file.path("..","tests",paste(name,".R",sep=""))
         if(file.exists(tfile))
           list(examples=paste(readLines(tfile),collapse="\n"))
         else list()
       }),
       ## Get examples from inline definitions after return()
       examples.after.return=list(forfun,function(name,src,...){
         coll <- paste(src,collapse="\n")
         thispre <- gsub("^[\\^]","",prefix)
         FIND <- paste("(return|UseMethod)[(][^\\n]*\\n",thispre,sep="")
         m <- regexpr(FIND,coll)
         if(m[1]==-1)return(list())
         after <- substr(coll,m[1],nchar(coll))
         FIND <-
           paste("[^\\n]*",# rest of the return line
                 "((?:\\n###[^\\n]*)+)",#comment value lines \\1
                 "([\\w\\W]*)[}]",#examples \\2
                 sep="")
         SEP <- "----------"
         REP <- paste("\\1",SEP,"\\2",sep="")
         r <- strsplit(gsub(FIND,REP,after,perl=TRUE),split=SEP)[[1]]
         l <- strsplit(r,split="\n")
         ##if(name=="dl.combine")browser()
         excode <- c(l[[2]],"")
         prefixes <- gsub("(\\s*).*","\\1",excode,perl=TRUE)[grep("\\w",excode)]
         FIND <- prefixes[which.min(nchar(prefixes))]
         list(examples=paste(sub(FIND,"",excode),collapse="\n"),
              value=decomment(l[[1]][-1]))
       }))

### List of parser functions that operate on single objects. This list
### is useful for testing these functions, ie
### lonely$parsefuns(attr(extract.docs.file,"src"),"extract.docs.file")
lonely <- sapply(forall.parsers,function(L)L[[2]])

extract.docs.file <- function # Extract documentation from a file
### Parse R code to extract inline documentation from comments around
### each function. These are not able to be retreived simply by
### looking at the "source" attribute. This is a Parser Function that
### can be used in the parser list of package.skeleton.dx().
(code,
### Code lines in a character vector containing multiple R objects to
### parse for documentation.
 objs,
### The objects defined in the code.
 ...
### ignored
 ){
  parsed <- extract.file.parse(code)
  extract.docs.try <- function(o,on)
    {
      ## Note: we could use parsed information here too, but that
      ## would produce different results for setMethodS3 etc.
      doc <- list()
      if ( !is.null(parsed[[on]]) ){
        if ( !is.na(parsed[[on]]@code[1]) ){ # no code given for generics
          doc$definition <- paste(parsed[[on]]@code,collapse="\n")
        }
        if(!"description"%in%names(doc) && !is.na(parsed[[on]]@description) ){
          doc$description <- parsed[[on]]@description
        }
        if ( "setMethodS3" == parsed[[on]]@created ){
          pattern <- "^([^\\.]+)\\.(.*)$"
          doc$s3method=c(m1 <- gsub(pattern,"\\1",on,perl=TRUE),
              m2 <- gsub(pattern,"\\2",on,perl=TRUE))
          if ( 0 < length(grep("\\W",m1,perl=TRUE)) ){
            m1 <- paste("`",m1,"`",sep="")
          }
          cat("S3method(",m1,",",m2,")\n",sep="")
        }
      }
      if("title" %in% names(doc) && !"description" %in% names(doc) ){
        ## For short functions having both would duplicate, but a
        ## description is required. Therefore automatically copy title
        ## across to avoid errors at package build time.
        doc$description <- doc$title
      }
      doc
    }
  extract.docs <- function(on){
    res <- try({o <- objs[[on]]
                extract.docs.try(o, on)},FALSE)
    if(class(res)=="try-error"){
      cat("Failed to extract docs for: ",on,"\n\n")
      list()
    } else if(0 == length(res) && inherits(objs[[on]],"standardGeneric")){
      NULL
    } else if(0 == length(res) && "function" %in% class(o)
              && 1 == length(osource <- attr(o,"source"))
              && 1 == length(grep(paste("UseMethod(",on,")",sep="\""),osource))
              ){
      ## phew - this should only pick up R.oo S3 generic definitions like:
      ## attr(*, "source")= chr "function(...) UseMethod(\"select\")"
      NULL
    } else res
  }
  doc.names <- names(objs)
  res <- sapply(doc.names,extract.docs,simplify=FALSE)
  ## Special processing for S4 classes as they do not appear in normal ls()
  for ( nn in names(parsed) ){
    if ( parsed[[nn]]@created == "setClass" ){
      S4class.docs <- extract.docs.setClass(parsed[[nn]])
      docname <- paste(nn,"class",sep="-")
      if ( is.null(res[[docname]]) ){
        res[[docname]] <- S4class.docs
        doc.names <- c(doc.names,docname)
      } else {
        stop(nn," appears as both S4 class and some other definition")
      }
    }
  }
  inherit.docs <- function(on){
    in.res <- res[[on]]
    if ( !is.null(parsed[[on]]) ){
      for ( parent in parsed[[on]]@parent ){
        if ( !is.na(parent) ){
          if ( is.null(in.res) ){
            in.res <- res[[parent]]
          } else if ( parent %in% names(res) ){
            parent.docs <- res[[parent]]
            for ( nn in names(parent.docs) ){
              if ( !nn %in% names(in.res) ){
                in.res[[nn]] <- parent.docs[[nn]]
              }
            }
          }
        }
      }
    }
    invisible(in.res)
  }
  all.done <- FALSE
  while ( !all.done ){
    res1 <- sapply(doc.names,inherit.docs,simplify=FALSE)
    all.done <- identical(res1,res)
    res <- res1
  }
  ## now strip out any generics (which have value NULL in res):
  res.not.null <- sapply(res,function(x){!is.null(x)})
  if ( 0 < length(res.not.null) && length(res.not.null) < length(res) ){
    res <- res[res.not.null]
  }
  res
### named list of lists, one for each object to document.
}

### List of parsers to use by default with package.skeleton.dx.
default.parsers <- c(extract.docs.file=extract.docs.file,
                     sapply(forall.parsers,function(L)L[[1]](L[[2]])))

extract.docs.fun <- function # Extract documentation from a function
### Given source code of a function, return a list describing inline
### documentation in that source code.
(code,
### The function to examine.
 name.fun
### The name of the function/chunk to use in warning messages.
 ){
  res <- list()
  clines <- grep(prefix,code)
  if(length(grep("#",code[1]))){
    res$title <- gsub("[^#]*#\\s*(.*)","\\1",code[1],perl=TRUE)
  }
  if(length(clines) > 0){
    ##details<<
    ## The primary mechanism is that consecutive groups of lines matching
    ## the specified prefix regular expression "\code{^### }" (i.e. lines
    ## beginning with "\code{### }") are collected
    ## as follows into documentation sections:\describe{
    ## \item{description}{group starting at line 2 in the code}
    ## \item{arguments}{group following each function argument}
    ## \item{value}{group ending at the penultimate line of the code}}
    ## These may be added to by use of the \code{##<<} constructs described
    ## below.
    bounds <- which(diff(clines)!=1)
    starts <- c(1,bounds+1)
    ends <- c(bounds,length(clines))
    for(i in seq_along(starts)){
      start <- clines[starts[i]]
      end <- clines[ends[i]]
      lab <- if(end+1==length(code))"value"
      else if(start==2)"description"
      else if ( 0 == length(grep("^\\s*#",code[start-1],perl=TRUE)) ){
         #arg <- gsub("^[ (]*","",code[start-1])
         #arg <- gsub("^([^=,]*)[=,].*","\\1",arg)
         #arg <- gsub("...","\\dots",arg,fix=TRUE) ##special case for dots
 		 arg <- gsub("^[ \t(,]*", "", code[start - 1])	#twutz: strip leading white spaces and brackets and ,
		 arg <- gsub("^([^=,]*)[=,].*", "\\1", arg)
		 arg <- gsub("^([^ \t]*)([ \t]+)$","\\1",arg)	#twutz: remove trailing whitespaces
		 arg <- gsub("...", "\\dots", arg, fix = TRUE)
         paste("item{",arg,"}",sep="")
       } else {
         next;
       }
      res[[lab]] <- decomment(code[start:end])
    }
  }
  ##details<< For simple functions/arguments, the argument may also be
  ## documented by appending \code{##<<} comments on the same line as the
  ## argument name. Mixing this mechanism with \code{###} comment lines for
  ## the same argument is likely to lead to confusion, as the \code{###}
  ## lines are processed first.
  #arg.pat <- paste("^[^=,#]*?([\\w\\.]+)\\s*([=,].*|\\)\\s*)?",
  #                 "<<\\s*(\\S.*?)\\s*$",
  #                 sep="##") # paste avoids embedded trigger fooling the system
   #tw: removed first comma
   arg.pat <- paste("^[^=#]*?([\\w\\.]+)\\s*([=,].*|\\)\\s*)?",
	   "<<\\s*(\\S.*?)\\s*$",
   		sep="##") # paste avoids embedded trigger fooling the system

  skeleton.fields <- c("alias","details","keyword","references","author",
                       "note","seealso","value","title","description",
                       "describe","end")
  ##details<< Additionally, consecutive sections of \code{##} comment
  ## lines beginning with \code{##}\emph{xxx}\code{<<} (where
  ## \emph{xxx} is one of the fields: \code{alias}, \code{details},
  ## \code{keyword}, \code{references}, \code{author}, \code{note},
  ## \code{seealso}, \code{value}, \code{title} or \code{description})
  ## are accumulated and inserted in the relevant part of the .Rd
  ## file.
  ##
  ## For \code{value}, \code{title}, \code{description} and function
  ## arguments, these \emph{append} to any text from "prefix"
  ## (\code{^### }) comment lines, irrespective of the order in the
  ## source.
  ##
  ## When documenting S4 classes, documentation from \code{details}
  ## sections will appear under a section \code{Objects from the Class}. That
  ## section typically includes information about construction methods
  ## as well as other description of class objects (but note that the
  ## class Slots are documented in a separate section).

  ## but this should not appear, because separated by a blank line
  extra.regexp <- paste("^\\s*##(",paste(skeleton.fields,collapse="|"),
                        ")<<\\s*(.*)$",sep="")
  cont.re <- "^\\s*##\\s*"
  in.describe <- 0
  first.describe <- FALSE
  k <- 1
  in.chunk <- FALSE
  end.chunk <- function(field,payload)
    {
      if ( "alias" == field ){
        ##note<< \code{alias} extras are automatically split at new lines.
        payload <- gsub("\\n+","\\}\n\\\\alias\\{",payload,perl=TRUE)
        chunk.sep <- "}\n\\alias{"
      } else if ( "keyword" == field ){
        ##keyword<< documentation utilities
        ##note<< \code{keyword} extras are automatically split at white space,
        ## as all the valid keywords are single words.
        payload <- gsub("\\s+","\\}\n\\\\keyword\\{",payload,perl=TRUE)
        chunk.sep <- "}\n\\keyword{"
      } else if ( "title" == field ){
        chunk.sep <- " "
      } else if ( "description" == field ){
        chunk.sep <- "\n"
      } else {
        ##details<< Each separate extra section appears as a new
        ## paragraph except that: \itemize{\item empty sections (no
        ## matter how many lines) are ignored;\item \code{alias} and
        ## \code{keyword} sections have special rules;\item
        ## \code{description} should be brief, so all such sections
        ## are concatenated as one paragraph;\item \code{title} should
        ## be one line, so any extra \code{title} sections are
        ## concatenated as a single line with spaces separating the
        ## sections.}
        chunk.sep <- "\n\n"
      }
      chunk.res <- NULL
      if ( 0 == length(grep("^\\s*$",payload,perl=TRUE)) )
        chunk.res <-
          if ( is.null(res[[field]]) ) payload
          else paste(res[[field]], payload, sep=chunk.sep)
      invisible(chunk.res)
    }
  while ( k <= length(code) ){
    line <- code[k]
    if ( 0 < length(grep(extra.regexp,line,perl=TRUE) ) ){
      ## we have a new extra chunk - first get field name and any payload
      new.field <- gsub(extra.regexp,"\\1",line,perl=TRUE)
      new.contents <- gsub(extra.regexp,"\\2",line,perl=TRUE)

      ##details<< As a special case, the construct \code{##describe<<} causes
      ## similar processing to the main function arguments to be
      ## applied in order to construct a describe block within the
      ## documentation, for example to describe the members of a
      ## list. All subsequent "same line" \code{##<<} comments go into that
      ## block until terminated by a subsequent \code{##}\emph{xxx}\code{<<} line.
      if ( "describe" == new.field ){
        ##details<< Such regions may be nested, but not in such a way
        ## that the first element in a \code{describe} is another \code{describe}.
        ## Thus there must be at least one \code{##<<} comment between each
        ## pair of \code{##describe<<} comments.
        if ( first.describe ){
          stop("consecutive ##describe<< at line",k,"in",name.fun)
        } else {
          if ( nzchar(new.contents) ){
            if ( is.null(payload) || 0 == nzchar(payload) ){
              payload <- new.contents
            } else {
              payload <- paste(payload,new.contents,sep="\n\n")
            }
          }
          first.describe <- TRUE
        }
      } else if ( "end" == new.field ){
        ##details<< When nested \code{describe} blocks are used, a comment-only
        ## line with \code{##end<<} terminates the current level only; any
        ## other valid \code{##}\emph{xxx}\code{<<} line terminates
        ## all open describe blocks.
        if ( in.describe>0 ){
          ## terminate current \item and \describe block only
          if ( "value" == cur.field && 1 == in.describe ){
            payload <- paste(payload,"}",sep="")
          } else {
            payload <- paste(payload,"}\n}",sep="")
          }
          in.describe <- in.describe-1;
        } else {
          warning("mismatched ##end<< at line ",k," in ",name.fun)
        }
        if ( nzchar(new.contents) ){
          if ( nzchar(payload) ){
            payload <- paste(payload,new.contents,sep="\n")
          } else {
            payload <- new.contents
          }
        }
      } else {
        ## terminate all open \describe blocks (+1 because of open item)
        if ( 0 < in.describe ){
          if ( "value" != cur.field ){  # value is implicit describe block
            payload <- paste(payload,"}",sep="")
          }
          while ( in.describe>0 ){
            payload <- paste(payload,"}",sep="\n")
            in.describe <- in.describe-1;
          }
        }
        ## finishing any existing payload
        if ( in.chunk ) res[[cur.field]] <- end.chunk(cur.field,payload)
        in.chunk <- TRUE
        cur.field <- new.field
        payload <- new.contents
        ##note<< The "value" section of a .Rd file is implicitly a describe
        ## block and \code{##}\code{value}\code{<<} acts accordingly. Therefore
        ## it automatically enables the describe block itemization (##<< after
        ## list entries).
        if ( "value" == new.field ){
          first.describe <- TRUE;
        }
      }
    } else if ( in.chunk && 0<length(grep(cont.re,line,perl=TRUE)) ){
      ## append this line to current chunk
      if ( 0 == length(grep(prefix,line,perl=TRUE)) ){
        ##describe<< Any lines with "\code{### }" at the left hand
        ## margin within the included chunks are handled separately,
        ## so if they appear in the documentation they will appear
        ## before the \code{##}\emph{xxx}\code{<}\code{<} chunks.
### This one should not appear.
        stripped <- gsub(cont.re,"",line,perl=TRUE)
        if ( nzchar(payload) ){
          payload <- paste(payload,stripped,sep="\n")
        } else {
          payload <- stripped
        }
      }
    } else if ( 0 < length(grep(arg.pat,line,perl=TRUE)) ){
      not.describe <- (0==in.describe && !first.describe)
      if ( in.chunk && not.describe){
        res[[cur.field]] <- end.chunk(cur.field,payload)
      }
      comment <- gsub(arg.pat,"\\3",line,perl=TRUE);
      arg <- gsub(arg.pat,"\\\\item\\{\\1\\}",line,perl=TRUE)
      in.chunk <- TRUE
      if ( not.describe ){
        cur.field <- gsub("...","\\dots",arg,fix=TRUE) ##special case for dots
        payload <- comment
      } else {
        ## this is a describe block, so we need to paste with existing
        ## payload as a new \item.
        if ( first.describe ){
          ## for first item, need to add describe block starter
          if ( "value" == cur.field ){
            payload <- paste(payload,"\n",arg,"{",sep="")
          } else {
            payload <- paste(payload,"\\describe{\n",arg,"{",sep="")
          }
          first.describe <- FALSE
          in.describe <- in.describe+1
        } else {
          ## subsequent item - terminate existing and start new
          payload <- paste(payload,"}\n",arg,"{",sep="")
        }
        if ( nzchar(comment) ){
          payload <- paste(payload,comment,sep="")
        }
      }
    } else if ( in.chunk ){
      if ( 0 == in.describe && !first.describe ){
        ## reached an end to current field, but need to wait if in.describe
        res[[cur.field]] <- end.chunk(cur.field,payload)
        in.chunk <- FALSE
        cur.field <- NULL
        payload <- NULL
      }
    }
    k <- k+1
  }
  ## finishing any existing payload
  if ( 0 < in.describe ){
    if ( "value" != cur.field ){    # value is implicit describe block
      payload <- paste(payload,"}",sep="")
    }
    while ( in.describe>0 ){
      payload <- paste(payload,"}",sep="\n")
      in.describe <- in.describe-1;
    }
  }
  if ( in.chunk ) res[[cur.field]] <- end.chunk(cur.field,payload)
  res
### Named list of character strings extracted from comments. For each
### name N we will look for N\{...\} in the Rd file and replace it
### with the string in this list (implemented in modify.Rd.file).
}

setClass("DocLink", # Link documentation among related functions
### The \code{.DocLink} class provides the basis for hooking together
### documentation of related classes/functions/objects. The aim is that
### documentation sections missing from the child are
         representation(name="character", ##<< name of object
                        created="character", ##<< how created
                        parent="character", ##<< parent class or NA
                        code="character", ##<< actual source lines
                        description="character") ##<< preceding description block
         )

extract.file.parse <- function # File content analysis
### Using the base \code{\link{parse}} function, analyse the file to link
### preceding "prefix" comments to each active chunk. Those comments form
### the default description for that chunk. The analysis also looks for
### S4 class "setClass" calls and R.oo setConstructorS3 and setMethodS3
### calls in order to link the documentation of those properly.
(code
### Lines of R source code in a character vector - note that any
### nested \code{source} statements are \emph{ignored} when scanning
### for class definitions.
 ){
  res <- list()
  old.opt <- options(keep.source=TRUE)
  parsed <- try(parse(text=code))
  options(old.opt)
  if ( inherits(parsed,"try-error") ){
    stop("parse failed with error:\n",parsed)
  }
  chunks <- attr(parsed,"srcref")
  last.end <- 0
  for ( k in 1:length(parsed) ){
    start <- chunks[[k]][1]
    ##details<< If the definition chunk does not contain a
    ## description, any immediately preceding sequence consecutive
    ## "prefix" lines will be used instead.
    default.description <- NULL
    while ( start > last.end+1
           && 1 == length(grep(prefix,code[start-1],perl=TRUE)) ){
      start <- start-1
    }
    if ( start < chunks[[k]][1] ){
      default.description <- decomment(code[start:(chunks[[k]][1]-1)])
    } else {
      default.description <- NA_character_;
    }
    ##details<< Class and method definitions can take several forms,
    ## determined by expression type: \describe{
    ## \item{assignment (<-)}{Ordinary assignment of value/function;}
    ## \item{setClass}{Definition of S4 class;}
    ## \item{setConstructorS3}{Definition of S3 class using R.oo package;}
    ## \item{setMethodS3}{Definition of method for S3 class using R.oo package.}}
    ## Additionally, the value may be a name of a function defined elsewhere,
    ## in which case the documentation should be copied from that other definition.
    ## This is handled using the concept of documentation links.
    lang <- parsed[[k]]
    chars <- as.character(lang)
    expr.type <- chars[1]
    parent <- NA_character_

    if ( expr.type == "<-" || expr.type == "setConstructorS3" || expr.type == "setClass" ){
      object.name <- chars[2]
      ## If the function definition is not embedded within the call, then
      ## the parent is that function. Test whether the the third value
      ## looks like a name and add it to parents if so.
      if ( 1 == length(grep("^[\\._\\w]+$",chars[3],perl=TRUE)) ){
        parent <- chars[3]
      }
      res[[object.name]] <- new("DocLink",name=object.name,
                                created=expr.type,
                                parent=parent,
                                code=paste(chunks[[k]],sep=""),
                                description=default.description)
    } else if ( expr.type == "setMethodS3" ){
      ##details<< The \code{setMethodS3} calls introduce additional
      ## complexity: they will define an additional S3 generic (which
      ## needs documentation to avoid warnings at package build time)
      ## unless one already exists. This also is handled by "linking"
      ## documentation. A previously unseen generic is linked to the
      ## first defining instances, subsequent definitions of that generic
      ## also link back to the first defining instance.
      generic.name <- chars[2]
      object.name <- paste(generic.name,chars[3],sep=".")
      if ( is.null(res[[generic.name]]) ){
        generic.desc <- paste("Generic method behind \\code{\\link{",object.name,"}}",sep="")
        res[[generic.name]] <- new("DocLink",
                                   name=generic.name,
                                   created=expr.type,
                                   parent=object.name,
                                   code=NA_character_,
                                   description=generic.desc)
      } else {
        parent <- res[[generic.name]]@parent
      }
      ## If the function definition is not embedded within the call, then
      ## the parent is that function. Test whether the the fourth value
      ## looks like a name and add it to parents if so.
      if ( 1 == length(grep("^[\\._\\w]+$",chars[4],perl=TRUE)) ){
        parent <- c(chars[4],parent)
      }
      res[[object.name]] <- new("DocLink",name=object.name,
                                created=expr.type,
                                parent=parent,
                                code=paste(chunks[[k]],sep=""),
                                description=default.description)
    } else {
      ## Not sure what to do with these yet. Need to deal with setMethod, setAs etc.
    }
  }
  invisible(res)
### Returns an invisible list of .DocLink objects.
}

extract.docs.setClass <- function # S4 class inline documentation
### Using the same conventions as for functions, definitions of S4 classes
### in the form \code{setClass("classname",\dots)} are also located and
### scanned for inline comments.
(doc.link
### DocLink object as created by \code{\link{extract.file.parse}}.
### Note that \code{source} statements are \emph{ignored} when scanning for
### class definitions.
 ){
  chunk.source <- doc.link@code
  ##details<<
  ## Extraction of S4 class documentation is currently limited to expressions
  ## within the source code which have first line starting with
  ## \code{setClass("classname"}. These are located from the source file
  ## (allowing also for white space around the \code{setClass} and \code{(}).
  ## Note that \code{"classname"} must be a quoted character string;
  ## expressions returning such a string are not matched.
  class.name <- doc.link@name

  ##details<< For class definitions, the slots (elements of the
  ## \code{representation} list) fill the role of function
  ## arguments, so may be documented by \code{##<<} comments on
  ## the same line or \code{### } comments at the beginning of the
  ## following line.
  f.n <- paste(class.name,"class",sep="-")
  docs <- extract.docs.fun(chunk.source,f.n)
  ##details<<
  ## The class definition skeleton includes an \code{Objects from the Class}
  ## section, to which any \code{##details<<} documentation chunks are
  ## written. It is given a vanilla content if there are no specific
  ## \code{##details<<} documentation chunks.
  if ( is.null(docs[["details"]]) ){
    docs[["details"]] <-
      paste("Objects can be created by calls of the form \\code{new(",
            class.name," ...)}",sep="")
  }
  docs[["section{Objects from the Class}"]] <- docs[["details"]]
  ## seealso has a skeleton line not marked by ~ .. ~, so have to suppress
  if ( is.null(docs[["seealso"]]) ){
    docs[["seealso"]] <- ""
  }
  if ( is.null(docs[["alias"]]) ){
    docs[["alias"]] <- class.name
  }
  if ( is.null(docs[["description"]]) ){
    docs[["description"]] <- doc.link@description
  }
  invisible(docs)
}

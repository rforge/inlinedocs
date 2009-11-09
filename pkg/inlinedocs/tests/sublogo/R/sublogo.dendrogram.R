read.fasta <- function
### Read sequences in FASTA format into a named character vector
(infile
### Name of the sequence file
 ){
  tmp <- sapply(strsplit(strsplit(paste('\n\n\n',paste(readLines(infile),collapse='\n'),sep=''),split='\n>')[[1]],'\n'),function(v)c(v[1],paste(v[2:length(v)],collapse='')))
  seqs <- tmp[2,]
  names(seqs) <- tmp[1,]
  blank <- names(seqs)==''
  names(seqs)[blank] <- seqs[blank]
  names(seqs) <- gsub(' ','',names(seqs))
  seqs[seqs!='']
}
##debug(read.fasta)

### Letters in the DNA alphabet, used to auto-detect sequence type
dna.letters <- c("*","A","T","G","C")
### DNA identity substitution matrix.
dna.identity <- matrix(0,nrow=length(dna.letters),ncol=length(dna.letters),
                       dimnames=list(dna.letters,dna.letters))
diag(dna.identity) <- 1
dna.identity['*','*'] <- 0
seqs.to.mat <- function
### Calculate pairwise differences between sequences using a
### substitution matrix.
(seq.vec,
### DNA or protein sequences.
 subs.mat=NULL){
### Substitution matrix with dimnames that match the letters used in
### the sequence data, or a character vector that specifies a common
### substitution matrix (as defined in the Biostrings package). NULL
### specifies that we will guess a suitable substitution matrix to
### match your input sequences (DNA=>identity, protein=>BLOSUM62).
  if(is.null(names(seq.vec)))names(seq.vec) <- seq.vec
  chars <- sapply(seq.vec,nchar)
  seqsum <- table(chars)
  if(length(seqsum)>1){
    print(as.data.frame(seqsum))
    i <- which.max(seqsum)
    cat("Sequences not of length ",names(seqsum)[i],":\n",sep="")
    rows <- as.integer(names(seqsum)[i])!=chars
    print(data.frame(chars,name=names(seq.vec),row.names=NULL)[rows,])
    stop("All input sequences must be of the same length.")
  }
  d <- toupper(gsub('[- .]',"*",seq.vec[unique(names(seq.vec))]))
  print(d)
  letters <- unique(c(unlist(strsplit(d,split='')),dna.letters))
  ##if dna alignment use simple identity matrix
  looks.like.dna <- identical(sort(letters),sort(dna.letters))
  if(is.null(subs.mat))
    subs.mat <- if(looks.like.dna)dna.identity else "BLOSUM62"
  if(mode(subs.mat)=="character"){
    ex <- substitute(data(M,package="Biostrings"),list(M=subs.mat))
    eval(ex)
    subs.mat <- get(subs.mat)
  }
  print(subs.mat)
  N <- length(d)
  m <- matrix(0,nrow=N,ncol=N,dimnames=list(names(d),names(d)))
  for(i in 1:N)for(j in 1:i){
    seqs <- sapply(strsplit(c(d[i],d[j]),split=''),c)
    entry <- try(apply(seqs,1,function(x)subs.mat[x[1],x[2]]))
    if(class(entry)=="try-error"){
      print(seqs)
      stop("Sequence difference matrix construction failed.")
    }
    ## subscript out of bounds here usually means bad matrix
    m[i,j] <- m[j,i] <- -sum(entry)
  }
  m <- m-min(m)
  attr(m,'seqs') <- d
  print(m[1:min(5,nrow(m)),1:min(5,ncol(m))])
  m
### The matrix of distances between each input sequence, with dimnames
### corresponding to either the sequences, or the sequence names (if
### they exist)
}
##debug(seqs.to.mat)

make.logo.ps <- function
### Create a logo using weblogo, then read it in using grImport
(helices,
### Sequences to plot in the logo
 psbase
### Base filename for the logo postscript and xml files, should be the
### full path name except for the trailing .ps
 ){
  psfile <- paste(psbase,'ps',sep='.')
  xmlfile <- paste(psfile,'xml',sep='.')
  seq.text <- paste(paste('>',helices,'\n',helices,sep=''),collapse='\n')
  write(seq.text,psbase)
  execdir <- system.file("exec",package="sublogo")
  cmd <- paste("PATH=",execdir,
               ":$PATH seqlogo -c -F EPS -f ",psbase,
               "|sed 's/^EndLine/%EndLine/'|sed 's/^EndLogo/%EndLogo/' >",
               psfile,sep="")
  cat(cmd,'\n')
  system(cmd)
  owd <- setwd(tempdir())
  PostScriptTrace(psfile,xmlfile)
  setwd(owd)
  pic <- readPicture(xmlfile)
  pic
### Grid picture grob as read using readPicture
}
##debug(make.logo.ps)

sublogo.dendrogram <- function
### Main function for drawing sublogo dendrograms.
(M,
### difference matrix as constructed by seqs.to.mat (although in
### principle any object with a valid as.dist method could be used)
 main='',
### plot title
 subtit=NULL,
### plot subtitle
 base=tempfile(),
### base file name for temporary logo files
 cutline=150,
### Distance for cutting the tree. Draw a sublogo for each
### leaf. Normally you will plot once, then inspect the dendrogram to
### determine which is a good value for cutline, then plot again using
### your chosen cutline.
 dend.width=30,
### Percent of the plot to be occupied by the dendrogram. The logos
### will be displayed with equal widths on either side.
 cex=1
### character expansion factor for the dendrogram
 ){
  hc <- hclust(as.dist(M),method="average")
  dend <- as.dendrogram(hc)
  fam <- cutree(hc,h=cutline)[labels(dend)] # order by plotting method
  famids <- unique(fam)
  famtab <- data.frame(fam,y=1:length(fam))
  famtab$seq <- attr(M,'seqs')[rownames(famtab)]
  fam.nontriv <- sapply(famids,function(i)sum(famtab$fam==i))>1
  names(fam.nontriv) <- famids
  if(is.null(subtit))
    subtit <- paste(nrow(M),"sequences,",sum(fam.nontriv),"families")
  xrange <- c(0,1)
  yrange <- c(0,max(famtab[,'y'])+1)
  draw.box <- function(i){ # replace with logos
    subtab <- famtab[famtab[,'fam']==i,]
    grprange <- range(subtab[,'y'])
    rect(xrange[1],grprange[1]-0.25,xrange[2],grprange[2]+0.25)
  }
  draw.logo <- function(i){
    ## do not draw sublogo for a family of trivial size
    if(fam.nontriv[as.character(i)]){ 
      subtab <- famtab[famtab[,'fam']==i,]
      grprange <- range(subtab[,'y'])
      tmpfile <- paste(base,i,sep='.')
      logo <- make.logo.ps(subtab$seq,tmpfile)
      grid.picture(logo,
                   x=xrange[1],
                   y=unit(grprange[1]+0.25,'native'),
                   height=unit(diff(grprange)-0.5,'native'),
                   distort=TRUE,
                   just=c(0,0),
                   fillText=TRUE)
    }
  }

  bottomspace <- 0.6
  topspace <- 0.4
  side.percents <- (100-dend.width)/2
  layout(matrix(1:3,ncol=3),c(side.percents,dend.width,side.percents))
  par(mai=c(bottomspace,0,topspace,0),cex=1)
  
  ## Big summary logo on left
  plot(xrange,yrange,
       bty='n',xaxt='n',yaxt='n',ylab='',xlab='',type='n',yaxs='i')
  biglogo <- make.logo.ps(famtab$seq,paste(base,'0',sep='.'))
  vps <- baseViewports()
  pushViewport(vps$inner,vps$figure,vps$plot)
  grid.picture(biglogo,
               y=unit(0,'native'),
               height=unit(length(fam),'native'),
               just=c(0.5,0),
               exp=0,
               distort=TRUE,
               fillText=TRUE)
  popViewport(3)
  
  ## Dendrogram in middle
  par(mai=c(bottomspace,0,topspace,
        max(strwidth(colnames(M),'inches',
                     cex))/6*5),
      family='mono')
  par(xpd=NA)
  plot(dend,h=TRUE,edgePar=list(lwd=2),nodePar=list(lab.cex=cex,pch=""))
  par(family="")
  segments(cutline,1,cutline,length(fam))
  ##axis(3,cutline,lty=0,line=0)
  
  ## Title in the middle
  title(main,line=0.5)
  mtext(subtit,line=-0.7,cex=1)

  # Sublogos on the right side
  par(mai=c(bottomspace,0,topspace,0))
  plot(xrange,yrange,
       bty='n',xaxt='n',yaxt='n',ylab='',xlab='',type='n',yaxs='i')
  vps <- baseViewports()
  pushViewport(vps$inner,vps$figure,vps$plot)
  sapply(famids,draw.logo)
  popViewport(3)
  
  hc
### The dendrogram from the call to hclust
}
##debug(sublogo.dendrogram)

sublogo <- function
### Draw a sublogo dendrogram for a sequence motif.
(seqs,
### Character vector of DNA or protein sequences (optionally named
### with sequence labels).
 mat=NULL,
### Substitution matrix passed to seqs.to.mat.
 ...
### Other arguments to pass to sublogo.dendrogram (see that help page
### for a full description).
 ){
  sublogo.dendrogram(seqs.to.mat(seqs,mat),...)
}

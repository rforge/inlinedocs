str_match_perl <- function(string,pattern){
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  if(is.null(attr(parsed,"capture.start")))return(matrix(captured.text,ncol=1))
  captured.groups <- do.call(rbind,lapply(seq_along(string),function(i){
    st <- attr(parsed,"capture.start")[i,]
    if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
    substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
  }))
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}
fmt <- "http://r-forge.r-project.org/scm/?group_id=%d"
home <- readLines(url("http://r-forge.r-project.org/"))
lines.after.recently <- home[-(1:grep("Recently",home))]
project.anchor <-
  paste('<a href="',
        '(?<url>https?://r-forge.r-project.org/projects/',
        '(?<project>[a-z]+)',
        '/)">(?<project_name>[^<]+)</a>',
        sep="")
parsed <- str_match_perl(lines.after.recently,project.anchor)
parsed[!is.na(parsed[,2]),]
most.recent.project <- parsed[!is.na(parsed[,2]),"project"][1]
## now download the most recent project page to get its project id
download.project.html <- function(project){
  project.url.format <- "http://r-forge.r-project.org/projects/%s/"
  project.url <- sprintf(project.url.format,project)
  conn <- url(project.url)
  html <- readLines(conn)
  close(conn)
  html
}
project.html <- download.project.html(most.recent.project)
parsed <- str_match_perl(project.html,"group_id=(?<group_id>[0-9]+)")
most.recent.project.id <- as.integer(parsed[!is.na(parsed[,1]),"group_id"][1])
## test parsing text strings into dates and plotting
datestr <- c("2011-05-30 18:43","2010-05-01 01:01")
test <- as.POSIXct(strptime(datestr,"%Y-%m-%d %H:%M"))
xyplot(y~date,data.frame(date=test,y=rnorm(length(test))))
## extract the project registration datetime
grep("Registered",project.html,value=TRUE)
parsed <- str_match_perl(project.html,"Registered:&nbsp;(?<datetime>[^<]+)")
register.string <- parsed[!is.na(parsed[,1]),"datetime"][1]
register.datetime <- as.POSIXct(strptime(register.string,"%Y-%m-%d %H:%M"))

## collect the data into a data.frame
project.stats <- data.frame(registered=register.datetime,
                            project=most.recent.project,
                            id=most.recent.project.id,
                            row.names=NULL)
## also collect the users info from this page
str_match_perl_all <- function(string,pattern){
  parsed <- gregexpr(pattern,string,perl=TRUE)
  lapply(seq_along(parsed),function(i){
    r <- parsed[[i]]
    starts <- attr(r,"capture.start")
    if(r[1]==-1)return(matrix(nrow=0,ncol=1+ncol(starts)))
    names <- attr(r,"capture.names")
    lengths <- attr(r,"capture.length")
    full <- substring(string[i],r,r+attr(r,"match.length")-1)
    subs <- substring(string[i],starts,starts+lengths-1)
    m <- matrix(c(full,subs),ncol=length(names)+1)
    colnames(m) <- c("",names)
    m
  })
}
user.pattern <- "org/users/(?<id>[a-z]+)/\">(?<name>[^<]+)"
inlinedocs.html <- download.project.html("inlinedocs")
parsed <- str_match_perl_all(inlinedocs.html,user.pattern)
user <- parsed[sapply(parsed,nrow)>0][[1]][,"id"]
project.users <- data.frame(user,project=most.recent.project,row.names=NULL)

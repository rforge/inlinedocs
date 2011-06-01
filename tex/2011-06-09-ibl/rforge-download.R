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
get.registration <- function(html){
  parsed <- str_match_perl(html,"Registered:&nbsp;(?<datetime>[^<]+)")
  register.string <- parsed[!is.na(parsed[,1]),"datetime"][1]
  as.POSIXct(strptime(register.string,"%Y-%m-%d %H:%M"))
}
register.datetime <- get.registration(project.html)
## collect the data into a data.frame
project.stats <- data.frame(registered=register.datetime,
                            project=most.recent.project,
                            id=most.recent.project.id,
                            row.names=NULL)

## to parse user info off the project page we need this function
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
get.user.ids <- function(html){
  user.pattern <- "org/users/(?<id>[^/]+)/\">(?<name>[^<]+)"
  parsed <- str_match_perl_all(html,user.pattern)
  not.empty <- parsed[sapply(parsed,nrow)>0]
  found <- do.call(rbind,not.empty)
  found[,"id"]
}
inlinedocs.html <- download.project.html("inlinedocs")
get.user.ids(inlinedocs.html)

## collect the users info from the most recent project page
user <- get.user.ids(project.html)
project.users <- data.frame(user,project=most.recent.project,row.names=NULL)

### lookup a project name from id by looking at the scm page
get.project.from.id <- function(project.id){
  scm.url <- sprintf("http://r-forge.r-project.org/scm/?group_id=%d",project.id)
  print(scm.url)
  scm.html <- tryCatch({
    conn <- url(scm.url)
    html <- readLines(conn)
    close(conn)
    html
  },error=function(e)"")
  parsed <- str_match_perl(scm.html,"svnroot/(?<project>[^<]+)")
  matched <- which(!is.na(parsed[,1]))
  if(length(matched))parsed[matched[1],"project"] else NA
### project name or NA if we couldn't do the lookup.
}

## first, lookup all ids from 1 to the most recent, and if the id is
## not in the project.stats table, lookup the project name.
for(project.id in 1:most.recent.project.id){
  print(project.id)
  if(!project.id%in%project.stats$id){
    project <- get.project.from.id(project.id)
    if(!is.na(project)){
      newline <- data.frame(registered=NA,
                            project,
                            id=project.id,
                            row.names=NULL)
      print(newline)
      project.stats <- rbind(project.stats,newline)
    }
  }
}

## then, lookup all projects with missing registration info
for(project in with(project.stats,project[is.na(registered)])){
  print(project)
  project.html <- download.project.html(project)
  registered <- get.registration(project.html)
  print(registered)
  project.stats[project.stats$project==project,"registered"] <- registered
  user <- get.user.ids(project.html)
  if(!is.null(user)){
    newlines <- data.frame(user,project,row.names=NULL)
    print(newlines)
    project.users <- rbind(project.users,newlines)
  }
}

sorted.projects <- project.stats[order(project.stats$id),]
write.csv(sorted.projects,"project.stats.csv",row.names=FALSE,quote=FALSE)

write.csv(project.users,"project.users.csv",row.names=FALSE,quote=FALSE)

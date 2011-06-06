download <- function(u){
  conn <- url(u)
  lines <- suppressWarnings(tryCatch({
    paste(readLines(conn),collapse="")
  },error=function(e)""))
  close(conn)
  lines
}
packages <- download("http://cran.r-project.org/web/packages/")
source("regexp.R")
## kind of slow:
package.names <- str_match_all_perl(packages,"web/packages/([^/]+)")[[1]][,2]
stats <- data.frame(package=package.names,published=NA)
class(stats$published) <- "POSIXct"
for(pkg in package.names){
  i <- stats$package==pkg
  if(is.na(stats[i,"published"])){
    print(pkg)
    fmt <- "http://cran.r-project.org/src/contrib/Archive/%s/?C=M;O=A"
    html <- download(print(sprintf(fmt,pkg)))
    datetime <- if(html!=""){
      parsed <- get.first(html,"align=\"right\">","[0-9]+[^<]+")
      strptime(parsed,"%d-%b-%Y %H:%M")
    }else{
      fmt <- "http://cran.r-project.org/web/packages/%s/index.html"
      html <- download(print(sprintf(fmt,pkg)))
      parsed <- get.first(html,"Published:</td><td>","[^<]+")
      strptime(parsed,"%Y-%m-%d")
    }
    stats[i,"published"] <- as.POSIXct(datetime)
  }
}
stats$published <- strptime(format(stats$published),"%Y-%m-%d %H:%M")
write.csv(stats,"cran.csv",row.names=FALSE,quote=FALSE)


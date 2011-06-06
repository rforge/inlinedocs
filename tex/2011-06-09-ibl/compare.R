rforge <- read.csv("project.stats.csv",header=TRUE,
                            colClasses=c("POSIXct","factor","integer"))
rforge$count <- 1:nrow(rforge)

stats <- read.csv("cran.csv",header=TRUE,colClasses=c("factor","POSIXct"))
cran <- stats[order(stats$published),]
cran$count <- 1:nrow(cran)
cran$registered <- cran$published
cran$project <- cran$package
plotcols <- c("registered","project","count")
both <- rbind(data.frame(rforge[,plotcols],site="R-Forge"),
              data.frame(cran[,plotcols],site="CRAN"))
p <- xyplot(count~registered,both,groups=site,type="l")
library(directlabels)
direct.label(p)

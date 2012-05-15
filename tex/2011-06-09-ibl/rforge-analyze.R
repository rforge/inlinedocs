sorted.projects <- read.csv("project.stats.csv",header=TRUE,
                            colClasses=c("POSIXct","factor","integer"))
sorted.projects$count <- 1:nrow(sorted.projects)
filename <- format(Sys.time(),"%Y-%m-%d-R-Forge-project-registration-over-time.png")
png(filename,width=800)
par(mar=c(5,4,4,4))
plot(count~registered,sorted.projects,type="s",las=1,
     xlab="Date of project registration",
     ylab="Total number of projects on R-Forge")
title("Number of R-Forge projects is linearly increasing",line=3)
last.line <- tail(sorted.projects,1)
last.date <- last.line$registered
axis(3,last.date,format(last.date,"%e %B %Y"))
axis(4,last.line$count,las=1)
dev.off()

users <- read.csv("project.users.csv",header=TRUE,
                  colClasses=c("factor","factor"))
users.per.project <- sort(table(users$project))
freq <- table(users.per.project)
library(xtable)
print(xtable(freq),floating=FALSE)

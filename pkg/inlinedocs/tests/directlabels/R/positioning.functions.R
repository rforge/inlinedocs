label.positions <- function
### Calculates table of positions of each label. It does not draw
### anything, but is called for its return value. Normally you don't
### have to call label.positions explicitly. Instead, it is called for
### you by direct.label, for each panel.
(x,
### x values of points to draw.
 y,
### y values of points to draw.
 subscripts,
### Subscripts of groups to consider.
 groups,
### Vector of groups.
 debug=FALSE,
### Show debug output? If TRUE, the resulting table of label positions
### will be printed.
 method,
### Method for direct labeling, specified in one of the following
### ways: (1) a Positioning Function, (2) the name of a Positioning
### Function as a character string, or (3) a list containing any
### number of (1), (2), or additionally named values. Starting from
### the data frame of points to plot for the panel, the elements of
### the list are applied in sequence, and each row of the resulting
### data frame is used to draw a direct label. See examples in
### ?direct.label and ?positioning.functions.
 ...
### Passed to Positioning Function(s).
 ){
  groups <- as.factor(groups)
  levs <- levels(groups)
  groups <- groups[subscripts]
  d <- data.frame(x,groups)
  if(!missing(y))d$y <- y
  if(class(method)=="function")method <- list(method)
  for(m.num in seq_along(method)){
    m <- method[[m.num]]
    m.var <- names(method)[m.num]
    if(!(is.null(m.var)||m.var==""))d[[m.var]] <- m else{
      if(class(m)=="character"){
        method.name <- paste(m," ",sep="")
        m <- get(m)
      }else method.name <- ""
      d <- try(m(d,debug=debug,...))
      if(class(d)=="try-error")
        stop("direct label placement method ",method.name,"failed")
    }
  }
  ## rearrange factors in case pos fun messed up the order:
  d$groups <- factor(as.character(d$groups),levs)
  ## defaults for grid parameter values:
  for(p in c("hjust","vjust"))
    d[,p] <- if(p %in% names(d))as.character(d[,p]) else 0.5
  if(!"rot"%in%names(d))d$rot <- 0
  d <- unique(d)
  if(debug)print(d)
  d
### Data frame of direct label positions. Each row describes the
### position of 1 label to be drawn later.
}

perpendicular.lines <- function
### Draw a line between the centers of each cluster, then draw a
### perpendicular line for each cluster that goes through its
### center. For each cluster, return the point the lies furthest out
### along this line.
(d,
### Data frame with groups x y.
 debug=FALSE,
### If TRUE will draw points at the center of each cluster and some
### lines that show how the points returned were chosen.
 ...
### ignored.
 ){
  means <- get.means(d)
  names(means)[2:3] <- c("mx","my")
  big <- merge(d,means,by="groups")
  fit <- lm(my~mx,means)
  b <- coef(fit)[1]
  m <- coef(fit)[2]
  big2 <- transform(big,x1=(mx+x+(my-y)*m)/2)
  big3 <- transform(big2,y1=m*(x1-x)+y)
  big4 <- transform(big3,
                    d=sqrt((x-x1)^2+(y-y1)^2),
                    dm=sqrt((x-mx)^2+(y-my)^2))
  big5 <- transform(big4,ratio=d/dm)
  winners <- ddply(big5,.(groups),subset,
                   subset=seq_along(ratio)==which.min(ratio))
  ## gives back a function of a line that goes through the designated center
  f <- function(v)function(x){
    r <- means[means$groups==v,]
    -1/m*(x-r$mx)+r$my
  }
  ##dd <- ddply(means,.(groups),summarise,x=x+sdx*seq(0,-2,l=5)[-1])
  ##dd$y <- mdply(dd,function(groups,x)f(groups)(x))$x
  if(debug){
    ## First find the mean of each cluster
    grid.points(means$mx,means$my,default.units="native")
    ## myline draws a line over the range of the data for a given fun F
    myline <- function(F)
      grid.lines(range(d$x),F(range(d$x)),default.units="native")
    ## Then draw a line between these means
    myline(function(x)m*x+b)
    ## Then draw perpendiculars that go through each center
    for(v in means$groups)myline(f(v))
  }
  winners[,c("x","y","groups")]
### Data frame with groups x y, giving the point for each cluster
### which is the furthest out along the line drawn through its center.
}
empty.grid <- function
### Label placement method for scatterplots that ensures labels are
### placed in different places. A grid is drawn over the whole
### plot. Each cluster is considered in sequence and assigned to the
### point on this grid which is closest to the point given by
### loc.fun().
(d,
### Data frame of points on the scatterplot with columns groups x y.
 debug=FALSE,
### Show debugging info on the plot? This is passed to loc.fun.
 loc.fun=get.means,
### Function that takes d and returns a data frame with 1 column for
### each group, giving the point we will use to look for a close point
### on the grid, to put the group label.
 ...
### ignored.
 ){
  loc <- loc.fun(d,debug)
  NREP <- 10
  gl <- function(v){
    s <- seq(from=min(d[,v]),to=max(d[,v]),l=NREP)
    list(centers=s,diff=(s[2]-s[1])/2)
  }
  L <- sapply(c("x","y"),gl,simplify=FALSE)
  g <- expand.grid(x=L$x$centers,y=L$y$centers)
  g2 <- transform(g,
                  left=x-L$x$diff,
                  right=x+L$x$diff,
                  top=y+L$y$diff,
                  bottom=y-L$y$diff)
  inbox <- function(x,y,left,right,top,bottom)
    c(data=sum(d$x>left & d$x<right & d$y>bottom & d$y<top))
  count.tab <- cbind(mdply(g2,inbox),expand.grid(i=1:NREP,j=1:NREP))
  count.mat <- matrix(count.tab$data,nrow=NREP,ncol=NREP,
                      byrow=TRUE,dimnames=list(1:NREP,1:NREP))[NREP:1,]
  mode(count.mat) <- "character"
  g3 <- transform(subset(count.tab,data==0))
  res <- data.frame()
  for(v in loc$groups){
    r <- subset(loc,groups==v)
    len <- sqrt((r$x-g3$x)^2+(r$y-g3$y)^2)
    i <- which.min(len) ## the box to use for this group
    count.mat[as.character(g3[i,"j"]),
              as.character(g3[i,"i"])] <- paste("*",v,sep="")
    res <- rbind(res,g3[i,c("x","y")])
    g3 <- g3[-i,]
  }
  if(debug)print(count.mat)
  cbind(res,groups=loc$groups)
### Data frame with columns groups x y, 1 line for each group, giving
### the positions on the grid closest to each cluster.
}
empty.grid.2 <- function
### Use the perpendicular lines method in combination with the empty
### grid method.
(d,
### Data frame with columns groups x y.
 debug,
### Show debugging graphics on the plot?
 ...
### ignored.
 ){
  empty.grid(d,debug,perpendicular.lines)
}
dl.indep <- function # Direct label groups independently
### Makes a function you can use to specify the location of each group
### independently.
(expr
### Expression that takes a subset of the d data frame, with data from
### only a single group, and returns the direct label position.
 ){
  foo <- substitute(expr)
  f <- function(d,...)eval(foo)
  src <- paste("dl.indep(",deparse(foo),")",sep="")
  structure(function(d,...)ddply(d,.(groups),f,...),"source"=src)
### A Positioning Function.
}
dl.trans <- function # Direct label data transform
### Make a function that transforms the data. This is for conveniently
### making a function that calls transform on the data frame, with the
### arguments provided. See examples.
(...
### Arguments to pass to transform.
 ){
  L <- as.list(match.call())[-1]
  function(d,...)do.call("transform",c(list(d),L))
### A Positioning Function.
}
### Transformation function for 1d densityplots.
trans.densityplot <- dl.indep({
  dens <- density(d$x)
  data.frame(x=dens$x,y=dens$y)
})
trans.density <- trans.densityplot
### Transformation function for 1d qqmath plots.
trans.qqmath <- dl.indep({
  r <- prepanel.default.qqmath(d$x,...)
  data.frame(x=r$x,y=r$y)
})
### Positioning Function for the first of a group of points.
first.points <-
  dl.indep(data.frame(d[which.min(d$x),],hjust=1,vjust=0.5))
left.points <- first.points
### Positioning Function for the last of a group of points.
last.points <-
  dl.indep(data.frame(d[which.max(d$x),],hjust=0,vjust=0.5))
right.points <- last.points
### Positioning Function for the top of a group of points.
top.points <-
  dl.indep(data.frame(d[which.max(d$y),],hjust=0.5,vjust=0))
high.points <- top.points
### Positioning Function for the bottom of a group of points.
bottom.points <-
  dl.indep(data.frame(d[which.min(d$y),],hjust=0.5,vjust=1))
low.points <- bottom.points
### Positioning Function for the mean of each cluster of points.
get.means <-
  dl.indep(data.frame(x=mean(d$x),y=mean(d$y)))
### Place points on top of the mean value of the rug.
rug.mean <- function(d,...,end)
  ddply(d,.(groups),function(d)
        data.frame(x=mean(d$x),
                   y=as.numeric(convertY(unit(end,"npc"),"native")),
                   vjust=0))
### Do first or last, whichever has points most spread out.
maxvar.points <- function(d,...){
  v <- ddply(d,.(x),summarise,v=var(y))
  x <- subset(v,v==max(v))$x
  if(x==min(d$x))first.points(d,...) else last.points(d,...)
}
direct.label <- function
### Add direct labels to a plot. This is a S3 generic and there are
### appropriate methods for "trellis" and "ggplot" objects.
(p,
### The plot to which you would like to add direct labels.
 method=NULL,
### The direct label placement method as described in
### ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  UseMethod("direct.label")
### The plot object, with direct labels added.
}
lines2 <- function
### Positioning Function for 2 groups of longitudinal data. One curve
### is on top of the other one (on average), so we label the top one
### at its maximal point, and the bottom one at its minimal
### point. Vertical justification is chosen to minimize collisions
### with the other line. This may not work so well for data with high
### variability, but then again lineplots may not be the best for
### these data either.
(d,
### The data.
 offset=0.3,
### Offset from 0 or 1 for the vjust values.
 ...
### ignored.
 ){
  top <- 0-offset
  bottom <- 1+offset
  y <- ddply(d,.(groups),function(d)mean(d$y))
  ddply(y,.(groups),function(D){
    biggest.on.average <- D$V==max(y$V)
    f <- if(biggest.on.average)max else min
    ld <- subset(d,groups==D$groups)
    pos <- ddply(subset(ld,y==f(ld$y)),.(groups),function(x)
          data.frame(x=max(x$x)-diff(range(x$x))/2,y=x$y[1]))
    other <- subset(d,groups!=D$groups)
    other.y <- other[which.min(abs(other$x-pos$x)),"y"]
    smaller.here <- pos$y<other.y
    data.frame(pos,vjust=if(biggest.on.average)
               if(smaller.here)bottom else top#bigger mean
               else if(smaller.here)bottom else top)#smaller mean
  })
}

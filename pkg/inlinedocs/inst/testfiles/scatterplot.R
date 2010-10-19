label.endpoints <- function
### Make a Positioning Method that labels a certain x value.
(FUN,
### FUN(d$x) should return an index of which point to label. for
### example you can use which.min or which.max.
 hjust
### hjust of the labels.
 ){
  function(d,...)ddply(d,.(groups),function(d,...){
    i <- FUN(d$x)
    if(length(i))data.frame(d[i,],hjust,vjust=0.5)
    else data.frame()
  })
### A Positioning Method like first.points or last.points.
}

dl.combine <- function # Combine output of several methods
### Apply several Positioning methods to the original data frame.
(...
### Several Positioning Functions.
 ){
  FUNS <- list(...)
  pf <- function(d,...){
    dfs <- lapply(FUNS,eval.list,d)
    res <- data.frame()
    for(df in dfs){
      if(nrow(res))res <- merge(df,res,all=TRUE)
      else res <- df
    }
    res
  }
  return(pf)
### A Positioning Function that returns the combined data frame after
### applying each specified Positioning Function.
  ##examples<<
  ## Simple example: label the start and endpoints
  data(BodyWeight,package="nlme")
  library(lattice)
  ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
  plot(direct.label(ratplot,dl.combine(first.points,last.points)))
  ## can also do this by repeatedly calling direct.label (ugly)
  plot(direct.label(direct.label(ratplot,last.points),first.points))
  library(ggplot2)
  rp2 <- qplot(Time,weight,data=BodyWeight,geom="line",facets=.~Diet,colour=Rat)
  print(direct.label(direct.label(rp2,last.points),first.points))

  mylars <- function
  ## Least angle regression algorithm for calculating lasso solutions.
  (x,
   ## Matrix of predictor variables.
   y,
   ## Vector of responses.
   epsilon=1e-6
   ## If correlation < epsilon, we are done.
   ){
    xscale <- scale(x) # need to work with standardized variables
    b <- rep(0,ncol(x))# coef vector starts at 0
    names(b) <- colnames(x)
    ycor <- apply(xscale,2,function(xj)sum(xj*y))
    j <- which.max(ycor) # variables in active set, starts with most correlated
    alpha.total <- 0
    out <- data.frame()
    
    while(1){## lar loop
      xak <- xscale[,j] # current variables
      r <- y-xscale%*%b # current residual
      ## direction of parameter evolution
      delta <- solve(t(xak)%*%xak)%*%t(xak)%*%r
      ## Current correlations (actually dot product)
      intercept <- apply(xscale,2,function(xk)sum(r*xk))
      ## current rate of change of correlations
      z <- xak%*%delta
      slope <- apply(xscale,2,function(xk)-sum(z*xk))
      ## store current values of parameters and correlation
      out <- rbind(out,data.frame(variable=colnames(x),
                                  coef=b,
                                  corr=abs(intercept),
                                  alpha=alpha.total,
                                  arclength=sum(abs(b)),
                                  coef.unscaled=b/attr(xscale,"scaled:scale")))

      if(sum(abs(intercept)) < epsilon)#corr==0 so we are done
        return(transform(out,s=arclength/max(arclength)))
      
      ## If there are more variables we can enter into the regression,
      ## then see which one will cross the highest correlation line
      ## first, and record the alpha value of where the lines cross.
      d <- data.frame(slope,intercept)
      d[d$intercept<0,] <- d[d$intercept<0,]*-1
      d0 <- data.frame(d[j[1],])# highest correlation line
      d2 <- data.frame(rbind(d,-d),variable=names(slope))#reflected lines
      ## Calculation of alpha for where lines cross for each variable
      d2$alpha <- (d0$intercept-d2$intercept)/(d2$slope-d0$slope)
      subd <- d2[(!d2$variable%in%colnames(x)[j])&d2$alpha>epsilon,]
      subd <- subd[which.min(subd$alpha),]
      nextvar <- subd$variable
      alpha <- if(nrow(subd))subd$alpha else 1
      
      ## If one of the coefficients would hit 0 at a smaller alpha
      ## value, take it out of the regression and continue.
      hit0 <- xor(b[j]>0,delta>0)&b[j]!=0
      alpha0 <- -b[j][hit0]/delta[hit0]
      takeout <- length(alpha0)&&min(alpha0) < alpha
      if(takeout){
        i <- which.min(alpha0)
        alpha <- alpha0[i]
      }
      
      b[j] <- b[j]+alpha*delta ## evolve parameters
      alpha.total <- alpha.total+alpha
      ## add or remove a variable from the active set
      j <- if(takeout)j[j!=which(names(i)==colnames(x))]
      else c(j,which(nextvar==colnames(x)))
    }
  }

  ## Calculate lasso path
  data(prostate,package="ElemStatLearn")
  pros <- subset(prostate,select=-train,train==TRUE)
  ycol <- which(names(pros)=="lpsa")
  x <- as.matrix(pros[-ycol])
  y <- unlist(pros[ycol])
  res <- mylars(x,y)
  P <- xyplot(coef~arclength,res,groups=variable,type="l")
  plot(direct.label(P,dl.combine(lasso.labels,last.qp)))

  data(diabetes,package="lars")
  dres <- with(diabetes,mylars(x,y))
  P <- xyplot(coef~arclength,dres,groups=variable,type="l")
  plot(direct.label(P,dl.combine(lasso.labels,last.qp)))
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
  src <- paste("dl.indep(",paste(deparse(foo),collapse="\n"),")",sep="")
  pf <- structure(function(d,...)ddply(d,.(groups),f,...),"source"=src)
  return(pf)
### A Positioning Function.
  ##examples<<
  complicated <- list(dl.trans(x=x+10),
                      dl.indep(d[-2,]),
                      rot=c(30,180))
  library(lattice)
  direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
}

dl.trans <- function # Direct label data transform
### Make a function that transforms the data. This is for conveniently
### making a function that calls transform on the data frame, with the
### arguments provided. See examples.
(...
### Arguments to pass to transform.
 ){
  L <- as.list(match.call())[-1]
  pf <- function(d,...)do.call("transform",c(list(d),L))
  return(pf)
### A Positioning Function.
  ##examples<<
  complicated <- list(dl.trans(x=x+10),
                      dl.indep(d[-2,]),
                      rot=c(30,180))
  library(lattice)
  direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
}

dl.move <- function # Manually move a direct label
### Sometimes there is 1 label that is placed oddly by another
### Positioning Function. This function can be used to manually place
### that label in a good spot.
(group,
### Group to change.
 x,
### Horizontal position of the new label.
 y,
### Vertical position of the new label. If missing(y) and !missing(x)
### then we will calculate a new y value using linear interpolation.
 ...
### Variables to change for the specified group
 ){
  L <- list(...)
  if(!missing(x))L$x <- x
  if(!missing(y))L$y <- y
  pf <- function(d,...){
    v <- d$groups==group
    for(N in names(L))
      d[v,N] <- L[[N]]
    ## maybe generalize this to be symmetric on x and y one day?
    if("x" %in% names(L) && (!"y" %in% names(L))){
      orig <- attr(d,"orig.data")
      orig <- orig[orig$groups==group,]
      ## do linear interpolation to find a good y-value
      f <- with(orig,approxfun(x,y))
      d[v,"y"] <- f(L$x)
    }
    d
  }
  return(pf)
### A Positioning Function that moves a label into a good spot.
  ##examples<<
  data(mpg,package="ggplot2")
  library(lattice)
  scatter <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1)
  dlcompare(list(scatter),
            list("extreme.grid",
                 `+dl.move`=list(extreme.grid,dl.move("suv",15,15))))

  data(svmtrain,package="directlabels")
  library(ggplot2)
  p <- qplot(log10(gamma),rate,data=svmtrain,group=data,colour=data,
             geom="line",facets=replicate~nu)
  dlcompare(list(p+xlim(-8,7)),list("last.points",
    `+dl.move`=list(last.points,dl.move("KIF11",-0.9,hjust=1,vjust=1))))
}

### Make a Positioning Function with empty.grid, that calculates label
### position targets using f.
empty.grid.fun <- function(f)
  function(d,debug,...)empty.grid(d,debug,f)

### Jitter the label positions.
dl.jitter <- dl.trans(x=jitter(x),y=jitter(y))

### Calculate boxes around labels, for collision detection.
calc.boxes <- function(d,debug=FALSE,...){
  vp <- current.viewport()
  convert <- function(worh){
    conv <- get(paste("convert",worh,sep=""))
    stri <- get(paste("string",worh,sep=""))
    with(d,sapply(seq_along(groups),function(i){
      if("cex"%in%names(d))vp$gp <- gpar(cex=cex[i])
      pushViewport(vp)
      if(debug)grid.rect() ##highlight current viewport
      w <- conv(stri(as.character(groups[i])),"native")
      popViewport()
      w
    }))
  }
  w <- convert("Width")
  h <- convert("Height")
  calc.borders(transform(d,w=w,h=h))
}

### Calculate big boxes around the means of each cluster.
big.boxes <- function(d,...)enlarge.box(calc.boxes(visualcenter(d)))

### Point in the middle of the min and max for each group.
visualcenter <-
  dl.indep(unique(transform(d,x=diff(range(x))/2+min(x),
                            y=diff(range(y))/2+min(y))))

### Positioning Function for the mean of each cluster of points.
get.means <-
  dl.indep(unique(transform(d,x=mean(x),y=mean(y))))

calc.borders <- function
### Calculate bounding box based on newly calculated width and height.
(d,
### Data frame of point labels, with new widths and heights in the w
### and h columns.
 ...
### ignored.
 ){
  hjust <- vjust <- 0.5 ##defaults in case unassigned in d
  transform(d,
            top=y+(1-vjust)*h,bottom=y-vjust*h,
            right=x+(1-hjust)*w,left=x-hjust*w,
            h=h,w=w)
}

### Positioning Function that draws boxes around label positions. Need
### to have previously called calc.boxes. Does not edit the data
### frame.
draw.rects <- function(d,...){
  ## easy way -- not correct, doesn't use calc'ed borders
  ##with(d,grid.rect(x,y,w,h,hjust=hjust,vjust=vjust,
  ##                 default.units="native",gp=gpar(col="grey")))
  d_ply(d,.(groups),function(D){
    with(D,grid.lines(c(left,left,right,right,left),
                      c(bottom,top,top,bottom,bottom),
                      "native",gp=gpar(col="grey")))
  })
  d
}

### Sequentially bump labels up, starting from the bottom, if they
### collide with the label underneath. NOTE: behavior is undefined
### when used with ggplot2 since it relies on the calc.boxes()
### function which doesn't know how to calculate bounding boxes for
### ggplot2 labels (yet).
bumpup <- function(d,...){
  d <- calc.boxes(d)[order(d$y),]
  "%between%" <- function(v,lims)lims[1]<v&v<lims[2]
  obox <- function(x,y){
    tocheck <- with(x,c(left,(right-left)/2+left,right))
    tocheck %between% with(y,c(left,right))
  }
  for(i in 2:nrow(d)){
    dif <- d$bottom[i]-d$top[i-1]
    ## here we are trying to test if box i can possibly collide with
    ## the box below it! Originally we checked if the bottom points of
    ## this box fall in the box below it, but this causes problems
    ## since we are reassigning box positions. If all boxes start at
    ## the same place, 2 will get moved up, 3 will not since its
    ## bottom points are no longer inside box 2. Solution: Look at box
    ## left and right limits and see if they collide!

    ## GOTCHA: If all the boxes are exactly the same size, on top of
    ## each other, then if we only examine left and right points of
    ## each box, none of the boxes will be detected as
    ## overlapping. One way to fix this is change > to >= in %between%
    ## but this is a bad idea since you can have boxes right next to
    ## each other that we don't want to move, that would be detected
    ## as overlapping. Solution: use the midpoint of the box as well!
    overlap <- c(obox(d[i,],d[i-1,]),obox(d[i-1,],d[i,]))
    if(dif<0&&any(overlap)){
      d$bottom[i] <- d$bottom[i]-dif
      d$top[i] <- d$top[i]-dif
      d$y[i] <- d$y[i]-dif
    }
  }
  d
}

### Use a QP solver to find the best places to put the points on a
### line, subject to the constraint that they should not overlap
qp.labels <- function(var,spacer)function(d,...){
  if(!spacer%in%names(d))stop("need to have calculated ",spacer)
  require(quadprog)
  d <- d[order(d[,var],decreasing=TRUE),]
  ## sorts data so that m_1 is on top, m_n on bottom.
  n <- nrow(d)
  D <- diag(rep(1,n))
  A <- diag(rep(1,n))[,-n]-rbind(0,diag(rep(1,n-1)))
  h <- d[,spacer]
  b0 <- (h[-n]+h[-1])/2
  sol <- solve.QP(D,d[,var],A,b0)
  d[,var] <- sol$solution
  d
}

### Make text bounding box larger by some amount.
enlarge.box <- function(d,...){
  if(!"h"%in%names(d))stop("need to have already calculated height and width.")
  h <- unit(d$h,"native")
  d$h <- d$h*2
  d$w <- d$w+as.numeric(convertWidth(convertHeight(h,"inches"),"native"))
  calc.borders(d)
}

in1which <- function
### Calculate which points fall in a box.
(p,
### data frame of points with columns x and y and many rows.
 box
### data frame of 1 row with columns left right top bottom.
 ){
  p$x>box$left & p$x<box$right & p$y<box$top & p$y>box$bottom
}

### Calculate how many points fall in a box.
in1box <- function(p,box)sum(in1which(p,box))

inside <- function
### Calculate for each box how many points are inside.
(boxes,
### Data frame of box descriptions, each row is 1 box, need columns
### left right top bottom.
 points
### Data frame of points, each row is 1 point, need columns x y.
 ){
  sapply(1:nrow(boxes),function(i)in1box(points,boxes[i,]))
### Vector of point counts for each box.
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
  means <- rename(get.means(d),list(x="mx",y="my",groups="groups"))
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

### Label the points furthest from the origin for each group.
extreme.points <- dl.indep({
  d <- transform(d,d=sqrt(x^2+y^2))
  d[which.max(d$d),]
})

edges.to.outside <- function
### Given a list of edges from the convex or alpha hull, and a list of
### cluster centers, calculate a point near to each cluster on the
### outside of the hull.
(edges,centers,debug=FALSE){
  if(debug){
    with(centers,lpoints(x,y,pch="+"))
    with(edges,lsegments(x1,y1,x2,y2))
  }
  closepts <- ddply(centers,.(groups),project.onto.segments,edges,debug)
  closepts$vjust <- ifelse(closepts$y-centers$y>0,0,1)
  closepts$hjust <- ifelse(closepts$x-centers$x>0,0,1)
  r <- big.boxes(closepts)
  transform(r,x=(right-left)/2+left,y=(top-bottom)/2+bottom,hjust=0.5,vjust=0.5)
}

project.onto.segments <- function
### Given a point and a set of line segments representing a convex or
### alpha hull, calculate the closest point on the segments.
(m,
### m is 1 row, a center of a point cloud, we need to find the
### distance to the closest point on each segment of the convex
### hull.
 hull.segments,
### Data frame describing the line segments of the convex or alpha
### hull.
 debug=FALSE
 ){
  these <- within(hull.segments,{
    s <- (y2-y1)/(x2-x1)
    ## the closest point on the line formed by expanding this line
    ## segment (this expression is calculated by finding the minimum
    ## of the distance function).
    xstar <- (m$x + m$y*s + x1*s^2 - s*y1)/(s^2+1)
    minval <- apply(cbind(x1,x2),1,min)
    maxval <- apply(cbind(x1,x2),1,max)
    ## xopt is the closest point on the line segment
    xopt <- ifelse(xstar<minval,minval,ifelse(xstar>maxval,maxval,xstar))
    yopt <- s*(xopt-x1)+y1
    ## distance to each point on line segment from the center
    d <- (m$x-xopt)^2+(m$y-yopt)^2
  })
  i <- which.min(these$d)
  h <- with(these[i,],data.frame(x=xopt,y=yopt))
  if(debug)with(h,lsegments(m$x,m$y,h$x,h$y))
  h
}

### Make a Positioning Method from a set of points on a vertical line
### that will be spaced out using qp.labels
vertical.qp <- function(M)list(M,calc.boxes,qp.labels("y","h"))

### Calculate closest point on the alpha hull with size of the boxes,
### and put it outside that point. (only technically correct for
### aspect="iso" TODO: check and correct for perspective changes.)
### TODO: doesn't work with ggplot2 since we can't calculate bounding
### box.
closest.on.ahull <- function(d,debug=FALSE,center.fun=big.boxes,...){
  require(alphahull)
  centers <- center.fun(d)
  alpha <- mean(unlist(centers[,c("w","h")]))/2
  as <- ashape(d[,c("x","y")],alpha=alpha)
  edges <- as.data.frame(as$edges)
  edges.to.outside(edges,centers,debug=debug)
}

### Calculate closest point on the convex hull and put it outside that
### point. TODO: doesn't work with ggplot2 since we can't calculate
### bounding box.
closest.on.chull <- function(d,debug=FALSE,center.fun=big.boxes,...){
  centers <- center.fun(d,debug=debug,...)
  bpts <- d[with(d,chull(x,y)),]
  edges <- transform(data.frame(i1=1:nrow(bpts),i2=c(2:nrow(bpts),1)),
                             x1=bpts$x[i1],
                             y1=bpts$y[i1],
                             x2=bpts$x[i2],
                             y2=bpts$y[i2])
  edges.to.outside(edges,centers,debug=debug)
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
  loc <- loc.fun(d,debug=debug)
  NREP <- 10
  gridpts <- d
  gl <- function(v){
    s <- seq(min(gridpts[,v]),max(gridpts[,v]),l=NREP)
    if(expand){
      dif <- s[2]-s[1]
      s <- seq(min(gridpts[,v])-expand*dif,
               max(gridpts[,v])+expand*dif,
               l=NREP+2*expand)
    }
    list(centers=s,diff=s[2]-s[1])
  }
  hgrid <- function(x,w){
    hboxes <- floor(diff(range(gridpts[,x]))/r[,w])
    (-expand:(hboxes+expand-1))*r[,w]+min(gridpts[,x])+r[,w]/2
  }
  if(debug)with(loc,grid.points(x,y,default.units="native"))
  draw <- function(g){
    gridlines <- with(g,list(x=unique(c(left,right)),y=unique(c(top,bottom))))
    drawlines <- function(a,b,c,d)
      grid.segments(a,b,c,d,"native",gp=gpar(col="grey"))
    with(gridlines,drawlines(min(x),y,max(x),y))
    with(gridlines,drawlines(x,min(y),x,max(y)))
  }
  res <- data.frame()
  for(v in loc$groups){
    r <- subset(loc,groups==v)
    no.points <- data.frame()
    expand <- 0
    while(nrow(no.points)==0){
      boxes <- if("left"%in%names(loc)){
        list(x=hgrid("x","w"),y=hgrid("y","h"),w=r$w,h=r$h)
      }else{
        L <- sapply(c("x","y"),gl,simplify=FALSE)
        list(x=L$x$centers,y=L$y$centers,w=L$x$diff,h=L$y$diff)
      }
      boxes <- calc.borders(do.call(expand.grid,boxes))
      boxes <- cbind(boxes,data=inside(boxes,d))
      no.points <- transform(subset(boxes,data==0))
      expand <- expand+1 ## look further out if we can't find any labels inside
    }
    if(debug)draw(boxes)
    no.points <- transform(no.points,len=(r$x-x)^2+(r$y-y)^2)
    best <- subset(no.points,len==min(len))[1,]
    res <- rbind(res,transform(r,x=best$x,y=best$y))
    ## add points to cloud
    newpts <- with(best,data.frame(x=c(left,left,right,right,x,x,x,left,right),
                                   y=c(bottom,top,top,bottom,top,bottom,y,y,y)))
    newpts <- data.frame(newpts,
                         subset(d,select=-c(x,y))[1,,drop=FALSE],
                         row.names=NULL)
    d <- rbind(d,newpts[,names(d)])
  }
  if(debug)with(d,grid.points(x,y))
  res
### Data frame with columns groups x y, 1 line for each group, giving
### the positions on the grid closest to each cluster.
}

### Use bounding box information with a small empty.grid to find the a
### non-colliding label that is close to a point on the convex hull,
### which is close to the visual center of the data. TODO: does not
### work with ggplot2 since the backend does not support bounding box
### calculation.
smart.grid <- empty.grid.fun(big.boxes)

### Use empty.grid with perpendicular.lines.
empty.grid.2 <- empty.grid.fun(perpendicular.lines)

### Use empty.grid with extreme.points.
extreme.grid <- empty.grid.fun(extreme.points)

follow.points <- function
### Draws a line between each center and every point, then follows the
### line out far enough to give a box outside the cloud. Out of all
### the boxes constructed in this way that do not contain any points,
### take the one which has the smallest distance to the center. FIXME:
### does not work with ggplot2 since the ggplot2 backend doesn't yet
### have support of actually knowing how big the text bounding box is.
(d,debug=FALSE,...){
  allm <- big.boxes(dl.jitter(d))
  if(debug)draw.rects(allm)
  labtab <- data.frame()
  for(g in levels(d$groups)){
    x <- d
    m <- subset(allm,groups==g)
    x$a <- x$y - m$y
    x$b <- x$x - m$x
    x$h <- sqrt(x$a^2+x$b^2) ## hypotenuse of triangle, not box height!
    x$x <- x$x + m$w/2 * x$b/x$h *1.01 ## b/h = cos(theta)
    x$y <- x$y + m$h/2 * x$a/x$h *1.01 ## a/h = sin(theta)
    x$dist <- (x$x-m$x)^2+(x$y-m$y)^2
    x <- transform(x,
                   left=x-m$w/2,right=x+m$w/2,
                   top=y+m$h/2,bottom=y-m$h/2)
    x$points <- inside(x,d)
    ## consider only subset of boxes that contain no points
    x <- subset(x,points==0)
    ## take the box with the minimal distance
    x <- subset(x,dist==min(dist))[1,]
    labtab <- rbind(labtab,transform(x,x=x,y=y,groups=g))
    ## add the box's 4 points to the list of points
    newpoints <- d[1:4,]
    newpoints$x <- c(x$left,x$right,x$right,x$left)
    newpoints$groups <- g
    newpoints$y <- c(x$top,x$top,x$bottom,x$bottom)
    d <- rbind(d,newpoints)
  }
  labtab
}

.result <- 
 list(big.boxes = list(definition = "big.boxes <- function(d,...)enlarge.box(calc.boxes(visualcenter(d)))",
        format="",title="big boxes",
     description = "Calculate big boxes around the means of each cluster."),  
     bumpup = list(definition = "bumpup <- function(d,...){\n  d <- calc.boxes(d)[order(d$y),]\n  \"%between%\" <- function(v,lims)lims[1]<v&v<lims[2]\n  obox <- function(x,y){\n    tocheck <- with(x,c(left,(right-left)/2+left,right))\n    tocheck %between% with(y,c(left,right))\n  }\n  for(i in 2:nrow(d)){\n    dif <- d$bottom[i]-d$top[i-1]\n    ## here we are trying to test if box i can possibly collide with\n    ## the box below it! Originally we checked if the bottom points of\n    ## this box fall in the box below it, but this causes problems\n    ## since we are reassigning box positions. If all boxes start at\n    ## the same place, 2 will get moved up, 3 will not since its\n    ## bottom points are no longer inside box 2. Solution: Look at box\n    ## left and right limits and see if they collide!\n\n    ## GOTCHA: If all the boxes are exactly the same size, on top of\n    ## each other, then if we only examine left and right points of\n    ## each box, none of the boxes will be detected as\n    ## overlapping. One way to fix this is change > to >= in %between%\n    ## but this is a bad idea since you can have boxes right next to\n    ## each other that we don't want to move, that would be detected\n    ## as overlapping. Solution: use the midpoint of the box as well!\n    overlap <- c(obox(d[i,],d[i-1,]),obox(d[i-1,],d[i,]))\n    if(dif<0&&any(overlap)){\n      d$bottom[i] <- d$bottom[i]-dif\n      d$top[i] <- d$top[i]-dif\n      d$y[i] <- d$y[i]-dif\n    }\n  }\n  d\n}",
       format="",title="bumpup",
         description = "Sequentially bump labels up, starting from the bottom, if they\ncollide with the label underneath. NOTE: behavior is undefined\nwhen used with ggplot2 since it relies on the calc.boxes()\nfunction which doesn't know how to calculate bounding boxes for\nggplot2 labels (yet)."),  
     calc.borders = list(definition = "calc.borders <- function\n### Calculate bounding box based on newly calculated width and height.\n(d,\n### Data frame of point labels, with new widths and heights in the w\n### and h columns.\n ...\n### ignored.\n ){\n  hjust <- vjust <- 0.5 ##defaults in case unassigned in d\n  transform(d,\n            top=y+(1-vjust)*h,bottom=y-vjust*h,\n            right=x+(1-hjust)*w,left=x-hjust*w,\n            h=h,w=w)\n}",  
         description = "Calculate bounding box based on newly calculated width and height.",
       format="",title="calc borders",
         `item{d}` = "Data frame of point labels, with new widths and heights in the w\nand h columns.",  
         `item{\\dots}` = "ignored."),
      calc.boxes = list(definition = "calc.boxes <- function(d,debug=FALSE,...){\n  vp <- current.viewport()\n  convert <- function(worh){\n    conv <- get(paste(\"convert\",worh,sep=\"\"))\n    stri <- get(paste(\"string\",worh,sep=\"\"))\n    with(d,sapply(seq_along(groups),function(i){\n      if(\"cex\"%in%names(d))vp$gp <- gpar(cex=cex[i])\n      pushViewport(vp)\n      if(debug)grid.rect() ##highlight current viewport\n      w <- conv(stri(as.character(groups[i])),\"native\")\n      popViewport()\n      w\n    }))\n  }\n  w <- convert(\"Width\")\n  h <- convert(\"Height\")\n  calc.borders(transform(d,w=w,h=h))\n}",
        format="",title="calc boxes",
         description = "Calculate boxes around labels, for collision detection."),  
     closest.on.ahull = list(definition = "closest.on.ahull <- function(d,debug=FALSE,center.fun=big.boxes,...){\n  require(alphahull)\n  centers <- center.fun(d)\n  alpha <- mean(unlist(centers[,c(\"w\",\"h\")]))/2\n  as <- ashape(d[,c(\"x\",\"y\")],alpha=alpha)\n  edges <- as.data.frame(as$edges)\n  edges.to.outside(edges,centers,debug=debug)\n}",
       format="",
       title="closest on ahull",
         description = "Calculate closest point on the alpha hull with size of the boxes,\nand put it outside that point. (only technically correct for\naspect=\"iso\" TODO: check and correct for perspective changes.)\nTODO: doesn't work with ggplot2 since we can't calculate bounding\nbox."),  
     closest.on.chull = list(definition = "closest.on.chull <- function(d,debug=FALSE,center.fun=big.boxes,...){\n  centers <- center.fun(d,debug=debug,...)\n  bpts <- d[with(d,chull(x,y)),]\n  edges <- transform(data.frame(i1=1:nrow(bpts),i2=c(2:nrow(bpts),1)),\n                             x1=bpts$x[i1],\n                             y1=bpts$y[i1],\n                             x2=bpts$x[i2],\n                             y2=bpts$y[i2])\n  edges.to.outside(edges,centers,debug=debug)\n}",  
         description = "Calculate closest point on the convex hull and put it outside that\npoint. TODO: doesn't work with ggplot2 since we can't calculate\nbounding box.",
      format="",
      title="closest on chull"),
     dl.combine = list(definition = "dl.combine <- function # Combine output of several methods\n### Apply several Positioning methods to the original data frame.\n(...\n### Several Positioning Functions.\n ){\n  FUNS <- list(...)\n  pf <- function(d,...){\n    dfs <- lapply(FUNS,eval.list,d)\n    res <- data.frame()\n    for(df in dfs){\n      if(nrow(res))res <- merge(df,res,all=TRUE)\n      else res <- df\n    }\n    res\n  }\n  return(pf)\n### A Positioning Function that returns the combined data frame after\n### applying each specified Positioning Function.\n  ##examples<<\n  ## Simple example: label the start and endpoints\n  data(BodyWeight,package=\"nlme\")\n  library(lattice)\n  ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))\n  plot(direct.label(ratplot,dl.combine(first.points,last.points)))\n  ## can also do this by repeatedly calling direct.label (ugly)\n  plot(direct.label(direct.label(ratplot,last.points),first.points))\n  library(ggplot2)\n  rp2 <- qplot(Time,weight,data=BodyWeight,geom=\"line\",facets=.~Diet,colour=Rat)\n  print(direct.label(direct.label(rp2,last.points),first.points))\n\n  mylars <- function\n  ## Least angle regression algorithm for calculating lasso solutions.\n  (x,\n   ## Matrix of predictor variables.\n   y,\n   ## Vector of responses.\n   epsilon=1e-6\n   ## If correlation < epsilon, we are done.\n   ){\n    xscale <- scale(x) # need to work with standardized variables\n    b <- rep(0,ncol(x))# coef vector starts at 0\n    names(b) <- colnames(x)\n    ycor <- apply(xscale,2,function(xj)sum(xj*y))\n    j <- which.max(ycor) # variables in active set, starts with most correlated\n    alpha.total <- 0\n    out <- data.frame()\n    \n    while(1){## lar loop\n      xak <- xscale[,j] # current variables\n      r <- y-xscale%*%b # current residual\n      ## direction of parameter evolution\n      delta <- solve(t(xak)%*%xak)%*%t(xak)%*%r\n      ## Current correlations (actually dot product)\n      intercept <- apply(xscale,2,function(xk)sum(r*xk))\n      ## current rate of change of correlations\n      z <- xak%*%delta\n      slope <- apply(xscale,2,function(xk)-sum(z*xk))\n      ## store current values of parameters and correlation\n      out <- rbind(out,data.frame(variable=colnames(x),\n                                  coef=b,\n                                  corr=abs(intercept),\n                                  alpha=alpha.total,\n                                  arclength=sum(abs(b)),\n                                  coef.unscaled=b/attr(xscale,\"scaled:scale\")))\n\n      if(sum(abs(intercept)) < epsilon)#corr==0 so we are done\n        return(transform(out,s=arclength/max(arclength)))\n      \n      ## If there are more variables we can enter into the regression,\n      ## then see which one will cross the highest correlation line\n      ## first, and record the alpha value of where the lines cross.\n      d <- data.frame(slope,intercept)\n      d[d$intercept<0,] <- d[d$intercept<0,]*-1\n      d0 <- data.frame(d[j[1],])# highest correlation line\n      d2 <- data.frame(rbind(d,-d),variable=names(slope))#reflected lines\n      ## Calculation of alpha for where lines cross for each variable\n      d2$alpha <- (d0$intercept-d2$intercept)/(d2$slope-d0$slope)\n      subd <- d2[(!d2$variable%in%colnames(x)[j])&d2$alpha>epsilon,]\n      subd <- subd[which.min(subd$alpha),]\n      nextvar <- subd$variable\n      alpha <- if(nrow(subd))subd$alpha else 1\n      \n      ## If one of the coefficients would hit 0 at a smaller alpha\n      ## value, take it out of the regression and continue.\n      hit0 <- xor(b[j]>0,delta>0)&b[j]!=0\n      alpha0 <- -b[j][hit0]/delta[hit0]\n      takeout <- length(alpha0)&&min(alpha0) < alpha\n      if(takeout){\n        i <- which.min(alpha0)\n        alpha <- alpha0[i]\n      }\n      \n      b[j] <- b[j]+alpha*delta ## evolve parameters\n      alpha.total <- alpha.total+alpha\n      ## add or remove a variable from the active set\n      j <- if(takeout)j[j!=which(names(i)==colnames(x))]\n      else c(j,which(nextvar==colnames(x)))\n    }\n  }\n\n  ## Calculate lasso path\n  data(prostate,package=\"ElemStatLearn\")\n  pros <- subset(prostate,select=-train,train==TRUE)\n  ycol <- which(names(pros)==\"lpsa\")\n  x <- as.matrix(pros[-ycol])\n  y <- unlist(pros[ycol])\n  res <- mylars(x,y)\n  P <- xyplot(coef~arclength,res,groups=variable,type=\"l\")\n  plot(direct.label(P,dl.combine(lasso.labels,last.qp)))\n\n  data(diabetes,package=\"lars\")\n  dres <- with(diabetes,mylars(x,y))\n  P <- xyplot(coef~arclength,dres,groups=variable,type=\"l\")\n  plot(direct.label(P,dl.combine(lasso.labels,last.qp)))\n}",
       format="",
         title = "Combine output of several methods", description = "Apply several Positioning methods to the original data frame.",  
         `item{\\dots}` = "Several Positioning Functions.",
         examples = "\n## Simple example: label the start and endpoints\ndata(BodyWeight,package=\"nlme\")\nlibrary(lattice)\nratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))\nplot(direct.label(ratplot,dl.combine(first.points,last.points)))\n## can also do this by repeatedly calling direct.label (ugly)\nplot(direct.label(direct.label(ratplot,last.points),first.points))\nlibrary(ggplot2)\nrp2 <- qplot(Time,weight,data=BodyWeight,geom=\"line\",facets=.~Diet,colour=Rat)\nprint(direct.label(direct.label(rp2,last.points),first.points))\n\nmylars <- function\n## Least angle regression algorithm for calculating lasso solutions.\n(x,\n ## Matrix of predictor variables.\n y,\n ## Vector of responses.\n epsilon=1e-6\n ## If correlation < epsilon, we are done.\n ){\n  xscale <- scale(x) # need to work with standardized variables\n  b <- rep(0,ncol(x))# coef vector starts at 0\n  names(b) <- colnames(x)\n  ycor <- apply(xscale,2,function(xj)sum(xj*y))\n  j <- which.max(ycor) # variables in active set, starts with most correlated\n  alpha.total <- 0\n  out <- data.frame()\n  \n  while(1){## lar loop\n    xak <- xscale[,j] # current variables\n    r <- y-xscale%*%b # current residual\n    ## direction of parameter evolution\n    delta <- solve(t(xak)%*%xak)%*%t(xak)%*%r\n    ## Current correlations (actually dot product)\n    intercept <- apply(xscale,2,function(xk)sum(r*xk))\n    ## current rate of change of correlations\n    z <- xak%*%delta\n    slope <- apply(xscale,2,function(xk)-sum(z*xk))\n    ## store current values of parameters and correlation\n    out <- rbind(out,data.frame(variable=colnames(x),\n                                coef=b,\n                                corr=abs(intercept),\n                                alpha=alpha.total,\n                                arclength=sum(abs(b)),\n                                coef.unscaled=b/attr(xscale,\"scaled:scale\")))\n\n    if(sum(abs(intercept)) < epsilon)#corr==0 so we are done\n      return(transform(out,s=arclength/max(arclength)))\n    \n    ## If there are more variables we can enter into the regression,\n    ## then see which one will cross the highest correlation line\n    ## first, and record the alpha value of where the lines cross.\n    d <- data.frame(slope,intercept)\n    d[d$intercept<0,] <- d[d$intercept<0,]*-1\n    d0 <- data.frame(d[j[1],])# highest correlation line\n    d2 <- data.frame(rbind(d,-d),variable=names(slope))#reflected lines\n    ## Calculation of alpha for where lines cross for each variable\n    d2$alpha <- (d0$intercept-d2$intercept)/(d2$slope-d0$slope)\n    subd <- d2[(!d2$variable%in%colnames(x)[j])&d2$alpha>epsilon,]\n    subd <- subd[which.min(subd$alpha),]\n    nextvar <- subd$variable\n    alpha <- if(nrow(subd))subd$alpha else 1\n    \n    ## If one of the coefficients would hit 0 at a smaller alpha\n    ## value, take it out of the regression and continue.\n    hit0 <- xor(b[j]>0,delta>0)&b[j]!=0\n    alpha0 <- -b[j][hit0]/delta[hit0]\n    takeout <- length(alpha0)&&min(alpha0) < alpha\n    if(takeout){\n      i <- which.min(alpha0)\n      alpha <- alpha0[i]\n    }\n    \n    b[j] <- b[j]+alpha*delta ## evolve parameters\n    alpha.total <- alpha.total+alpha\n    ## add or remove a variable from the active set\n    j <- if(takeout)j[j!=which(names(i)==colnames(x))]\n    else c(j,which(nextvar==colnames(x)))\n  }\n}\n\n## Calculate lasso path\ndata(prostate,package=\"ElemStatLearn\")\npros <- subset(prostate,select=-train,train==TRUE)\nycol <- which(names(pros)==\"lpsa\")\nx <- as.matrix(pros[-ycol])\ny <- unlist(pros[ycol])\nres <- mylars(x,y)\nP <- xyplot(coef~arclength,res,groups=variable,type=\"l\")\nplot(direct.label(P,dl.combine(lasso.labels,last.qp)))\n\ndata(diabetes,package=\"lars\")\ndres <- with(diabetes,mylars(x,y))\nP <- xyplot(coef~arclength,dres,groups=variable,type=\"l\")\nplot(direct.label(P,dl.combine(lasso.labels,last.qp)))\n",  
         value = "A Positioning Function that returns the combined data frame after\napplying each specified Positioning Function."),  
     dl.indep = list(definition = "dl.indep <- function # Direct label groups independently\n### Makes a function you can use to specify the location of each group\n### independently.\n(expr\n### Expression that takes a subset of the d data frame, with data from\n### only a single group, and returns the direct label position.\n ){\n  foo <- substitute(expr)\n  f <- function(d,...)eval(foo)\n  src <- paste(\"dl.indep(\",paste(deparse(foo),collapse=\"\\n\"),\")\",sep=\"\")\n  pf <- structure(function(d,...)ddply(d,.(groups),f,...),\"source\"=src)\n  return(pf)\n### A Positioning Function.\n  ##examples<<\n  complicated <- list(dl.trans(x=x+10),\n                      dl.indep(d[-2,]),\n                      rot=c(30,180))\n  library(lattice)\n  direct.label(dotplot(VADeaths,type=\"o\"),complicated,TRUE)\n}",
       examples="\ncomplicated <- list(dl.trans(x=x+10),\n                    dl.indep(d[-2,]),\n                    rot=c(30,180))\nlibrary(lattice)\ndirect.label(dotplot(VADeaths,type=\"o\"),complicated,TRUE)\n",
       format="",
         title = "Direct label groups independently", description = "Makes a function you can use to specify the location of each group\nindependently.",  
         `item{expr}` = "Expression that takes a subset of the d data frame, with data from\nonly a single group, and returns the direct label position.",  
         value = "A Positioning Function."),
      dl.jitter = list( format="",title="dl jitter",
         definition = "dl.jitter <- dl.trans(x=jitter(x),y=jitter(y))",  
         description = "Jitter the label positions."),
      dl.move = list( format="",
         definition = "dl.move <- function # Manually move a direct label\n### Sometimes there is 1 label that is placed oddly by another\n### Positioning Function. This function can be used to manually place\n### that label in a good spot.\n(group,\n### Group to change.\n x,\n### Horizontal position of the new label.\n y,\n### Vertical position of the new label. If missing(y) and !missing(x)\n### then we will calculate a new y value using linear interpolation.\n ...\n### Variables to change for the specified group\n ){\n  L <- list(...)\n  if(!missing(x))L$x <- x\n  if(!missing(y))L$y <- y\n  pf <- function(d,...){\n    v <- d$groups==group\n    for(N in names(L))\n      d[v,N] <- L[[N]]\n    ## maybe generalize this to be symmetric on x and y one day?\n    if(\"x\" %in% names(L) && (!\"y\" %in% names(L))){\n      orig <- attr(d,\"orig.data\")\n      orig <- orig[orig$groups==group,]\n      ## do linear interpolation to find a good y-value\n      f <- with(orig,approxfun(x,y))\n      d[v,\"y\"] <- f(L$x)\n    }\n    d\n  }\n  return(pf)\n### A Positioning Function that moves a label into a good spot.\n  ##examples<<\n  data(mpg,package=\"ggplot2\")\n  library(lattice)\n  scatter <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1)\n  dlcompare(list(scatter),\n            list(\"extreme.grid\",\n                 `+dl.move`=list(extreme.grid,dl.move(\"suv\",15,15))))\n\n  data(svmtrain,package=\"directlabels\")\n  library(ggplot2)\n  p <- qplot(log10(gamma),rate,data=svmtrain,group=data,colour=data,\n             geom=\"line\",facets=replicate~nu)\n  dlcompare(list(p+xlim(-8,7)),list(\"last.points\",\n    `+dl.move`=list(last.points,dl.move(\"KIF11\",-0.9,hjust=1,vjust=1))))\n}",  
         title = "Manually move a direct label", description = "Sometimes there is 1 label that is placed oddly by another\nPositioning Function. This function can be used to manually place\nthat label in a good spot.",  
         `item{group}` = "Group to change.", `item{x}` = "Horizontal position of the new label.",  
         `item{y}` = "Vertical position of the new label. If missing(y) and !missing(x)\nthen we will calculate a new y value using linear interpolation.",  
         `item{\\dots}` = "Variables to change for the specified group",  
         examples = "\ndata(mpg,package=\"ggplot2\")\nlibrary(lattice)\nscatter <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1)\ndlcompare(list(scatter),\n          list(\"extreme.grid\",\n               `+dl.move`=list(extreme.grid,dl.move(\"suv\",15,15))))\n\ndata(svmtrain,package=\"directlabels\")\nlibrary(ggplot2)\np <- qplot(log10(gamma),rate,data=svmtrain,group=data,colour=data,\n           geom=\"line\",facets=replicate~nu)\ndlcompare(list(p+xlim(-8,7)),list(\"last.points\",\n  `+dl.move`=list(last.points,dl.move(\"KIF11\",-0.9,hjust=1,vjust=1))))\n",  
         value = "A Positioning Function that moves a label into a good spot."),  
     dl.trans = list(definition = "dl.trans <- function # Direct label data transform\n### Make a function that transforms the data. This is for conveniently\n### making a function that calls transform on the data frame, with the\n### arguments provided. See examples.\n(...\n### Arguments to pass to transform.\n ){\n  L <- as.list(match.call())[-1]\n  pf <- function(d,...)do.call(\"transform\",c(list(d),L))\n  return(pf)\n### A Positioning Function.\n  ##examples<<\n  complicated <- list(dl.trans(x=x+10),\n                      dl.indep(d[-2,]),\n                      rot=c(30,180))\n  library(lattice)\n  direct.label(dotplot(VADeaths,type=\"o\"),complicated,TRUE)\n}",
       format="",
         title = "Direct label data transform", description = "Make a function that transforms the data. This is for conveniently\nmaking a function that calls transform on the data frame, with the\narguments provided. See examples.",  
         `item{\\dots}` = "Arguments to pass to transform.",
         examples = "\ncomplicated <- list(dl.trans(x=x+10),\n                    dl.indep(d[-2,]),\n                    rot=c(30,180))\nlibrary(lattice)\ndirect.label(dotplot(VADeaths,type=\"o\"),complicated,TRUE)\n",  
         value = "A Positioning Function."),
      draw.rects = list( format="",title="draw rects",
         definition = "draw.rects <- function(d,...){\n  ## easy way -- not correct, doesn't use calc'ed borders\n  ##with(d,grid.rect(x,y,w,h,hjust=hjust,vjust=vjust,\n  ##                 default.units=\"native\",gp=gpar(col=\"grey\")))\n  d_ply(d,.(groups),function(D){\n    with(D,grid.lines(c(left,left,right,right,left),\n                      c(bottom,top,top,bottom,bottom),\n                      \"native\",gp=gpar(col=\"grey\")))\n  })\n  d\n}",  
         description = "Positioning Function that draws boxes around label positions. Need\nto have previously called calc.boxes. Does not edit the data\nframe."),  
     edges.to.outside = list(definition = "edges.to.outside <- function\n### Given a list of edges from the convex or alpha hull, and a list of\n### cluster centers, calculate a point near to each cluster on the\n### outside of the hull.\n(edges,centers,debug=FALSE){\n  if(debug){\n    with(centers,lpoints(x,y,pch=\"+\"))\n    with(edges,lsegments(x1,y1,x2,y2))\n  }\n  closepts <- ddply(centers,.(groups),project.onto.segments,edges,debug)\n  closepts$vjust <- ifelse(closepts$y-centers$y>0,0,1)\n  closepts$hjust <- ifelse(closepts$x-centers$x>0,0,1)\n  r <- big.boxes(closepts)\n  transform(r,x=(right-left)/2+left,y=(top-bottom)/2+bottom,hjust=0.5,vjust=0.5)\n}",
       format="",
       title="edges to outside",
         description = "Given a list of edges from the convex or alpha hull, and a list of\ncluster centers, calculate a point near to each cluster on the\noutside of the hull."),  
     empty.grid = list(format="",title="empty grid",
       definition = "empty.grid <- function\n### Label placement method for scatterplots that ensures labels are\n### placed in different places. A grid is drawn over the whole\n### plot. Each cluster is considered in sequence and assigned to the\n### point on this grid which is closest to the point given by\n### loc.fun().\n(d,\n### Data frame of points on the scatterplot with columns groups x y.\n debug=FALSE,\n### Show debugging info on the plot? This is passed to loc.fun.\n loc.fun=get.means,\n### Function that takes d and returns a data frame with 1 column for\n### each group, giving the point we will use to look for a close point\n### on the grid, to put the group label.\n ...\n### ignored.\n ){\n  loc <- loc.fun(d,debug=debug)\n  NREP <- 10\n  gridpts <- d\n  gl <- function(v){\n    s <- seq(min(gridpts[,v]),max(gridpts[,v]),l=NREP)\n    if(expand){\n      dif <- s[2]-s[1]\n      s <- seq(min(gridpts[,v])-expand*dif,\n               max(gridpts[,v])+expand*dif,\n               l=NREP+2*expand)\n    }\n    list(centers=s,diff=s[2]-s[1])\n  }\n  hgrid <- function(x,w){\n    hboxes <- floor(diff(range(gridpts[,x]))/r[,w])\n    (-expand:(hboxes+expand-1))*r[,w]+min(gridpts[,x])+r[,w]/2\n  }\n  if(debug)with(loc,grid.points(x,y,default.units=\"native\"))\n  draw <- function(g){\n    gridlines <- with(g,list(x=unique(c(left,right)),y=unique(c(top,bottom))))\n    drawlines <- function(a,b,c,d)\n      grid.segments(a,b,c,d,\"native\",gp=gpar(col=\"grey\"))\n    with(gridlines,drawlines(min(x),y,max(x),y))\n    with(gridlines,drawlines(x,min(y),x,max(y)))\n  }\n  res <- data.frame()\n  for(v in loc$groups){\n    r <- subset(loc,groups==v)\n    no.points <- data.frame()\n    expand <- 0\n    while(nrow(no.points)==0){\n      boxes <- if(\"left\"%in%names(loc)){\n        list(x=hgrid(\"x\",\"w\"),y=hgrid(\"y\",\"h\"),w=r$w,h=r$h)\n      }else{\n        L <- sapply(c(\"x\",\"y\"),gl,simplify=FALSE)\n        list(x=L$x$centers,y=L$y$centers,w=L$x$diff,h=L$y$diff)\n      }\n      boxes <- calc.borders(do.call(expand.grid,boxes))\n      boxes <- cbind(boxes,data=inside(boxes,d))\n      no.points <- transform(subset(boxes,data==0))\n      expand <- expand+1 ## look further out if we can't find any labels inside\n    }\n    if(debug)draw(boxes)\n    no.points <- transform(no.points,len=(r$x-x)^2+(r$y-y)^2)\n    best <- subset(no.points,len==min(len))[1,]\n    res <- rbind(res,transform(r,x=best$x,y=best$y))\n    ## add points to cloud\n    newpts <- with(best,data.frame(x=c(left,left,right,right,x,x,x,left,right),\n                                   y=c(bottom,top,top,bottom,top,bottom,y,y,y)))\n    newpts <- data.frame(newpts,\n                         subset(d,select=-c(x,y))[1,,drop=FALSE],\n                         row.names=NULL)\n    d <- rbind(d,newpts[,names(d)])\n  }\n  if(debug)with(d,grid.points(x,y))\n  res\n### Data frame with columns groups x y, 1 line for each group, giving\n### the positions on the grid closest to each cluster.\n}",  
         description = "Label placement method for scatterplots that ensures labels are\nplaced in different places. A grid is drawn over the whole\nplot. Each cluster is considered in sequence and assigned to the\npoint on this grid which is closest to the point given by\nloc.fun().",  
         `item{d}` = "Data frame of points on the scatterplot with columns groups x y.",  
         `item{debug}` = "Show debugging info on the plot? This is passed to loc.fun.",  
         `item{loc.fun}` = "Function that takes d and returns a data frame with 1 column for\neach group, giving the point we will use to look for a close point\non the grid, to put the group label.",  
         `item{\\dots}` = "ignored.", value = "Data frame with columns groups x y, 1 line for each group, giving\nthe positions on the grid closest to each cluster."),  
     empty.grid.2 = list(definition = "empty.grid.2 <- empty.grid.fun(perpendicular.lines)",
       format="",title="empty grid 2",
         description = "Use empty.grid with perpendicular.lines."),  
     empty.grid.fun = list(definition = "empty.grid.fun <- function(f)\n  function(d,debug,...)empty.grid(d,debug,f)",
       format="",title="empty grid fun",
         description = "Make a Positioning Function with empty.grid, that calculates label\nposition targets using f."),  
     enlarge.box = list(definition = "enlarge.box <- function(d,...){\n  if(!\"h\"%in%names(d))stop(\"need to have already calculated height and width.\")\n  h <- unit(d$h,\"native\")\n  d$h <- d$h*2\n  d$w <- d$w+as.numeric(convertWidth(convertHeight(h,\"inches\"),\"native\"))\n  calc.borders(d)\n}",
       format="",
       title="enlarge box",
         description = "Make text bounding box larger by some amount."),  
     extreme.grid = list(definition = "extreme.grid <- empty.grid.fun(extreme.points)",
       format="",
       title="extreme grid",
         description = "Use empty.grid with extreme.points."),  
     extreme.points = list(definition = "extreme.points <- dl.indep({\n  d <- transform(d,d=sqrt(x^2+y^2))\n  d[which.max(d$d),]\n})",
       format="",title="extreme points",
         description = "Label the points furthest from the origin for each group."),  
     follow.points = list(definition = "follow.points <- function\n### Draws a line between each center and every point, then follows the\n### line out far enough to give a box outside the cloud. Out of all\n### the boxes constructed in this way that do not contain any points,\n### take the one which has the smallest distance to the center. FIXME:\n### does not work with ggplot2 since the ggplot2 backend doesn't yet\n### have support of actually knowing how big the text bounding box is.\n(d,debug=FALSE,...){\n  allm <- big.boxes(dl.jitter(d))\n  if(debug)draw.rects(allm)\n  labtab <- data.frame()\n  for(g in levels(d$groups)){\n    x <- d\n    m <- subset(allm,groups==g)\n    x$a <- x$y - m$y\n    x$b <- x$x - m$x\n    x$h <- sqrt(x$a^2+x$b^2) ## hypotenuse of triangle, not box height!\n    x$x <- x$x + m$w/2 * x$b/x$h *1.01 ## b/h = cos(theta)\n    x$y <- x$y + m$h/2 * x$a/x$h *1.01 ## a/h = sin(theta)\n    x$dist <- (x$x-m$x)^2+(x$y-m$y)^2\n    x <- transform(x,\n                   left=x-m$w/2,right=x+m$w/2,\n                   top=y+m$h/2,bottom=y-m$h/2)\n    x$points <- inside(x,d)\n    ## consider only subset of boxes that contain no points\n    x <- subset(x,points==0)\n    ## take the box with the minimal distance\n    x <- subset(x,dist==min(dist))[1,]\n    labtab <- rbind(labtab,transform(x,x=x,y=y,groups=g))\n    ## add the box's 4 points to the list of points\n    newpoints <- d[1:4,]\n    newpoints$x <- c(x$left,x$right,x$right,x$left)\n    newpoints$groups <- g\n    newpoints$y <- c(x$top,x$top,x$bottom,x$bottom)\n    d <- rbind(d,newpoints)\n  }\n  labtab\n}",
       format="",title="follow points",
         description = "Draws a line between each center and every point, then follows the\nline out far enough to give a box outside the cloud. Out of all\nthe boxes constructed in this way that do not contain any points,\ntake the one which has the smallest distance to the center. FIXME:\ndoes not work with ggplot2 since the ggplot2 backend doesn't yet\nhave support of actually knowing how big the text bounding box is."),  
     get.means = list(definition = "get.means <-\n  dl.indep(unique(transform(d,x=mean(x),y=mean(y))))",
       format="",
       title="get means",
         description = "Positioning Function for the mean of each cluster of points."),  
     in1box = list(definition = "in1box <- function(p,box)sum(in1which(p,box))",
       format="",title="in1box",
         description = "Calculate how many points fall in a box."),  
     in1which = list(definition = "in1which <- function\n### Calculate which points fall in a box.\n(p,\n### data frame of points with columns x and y and many rows.\n box\n### data frame of 1 row with columns left right top bottom.\n ){\n  p$x>box$left & p$x<box$right & p$y<box$top & p$y>box$bottom\n}",
       format="",title="in1which",
         description = "Calculate which points fall in a box.",  
         `item{p}` = "data frame of points with columns x and y and many rows.",  
         `item{box}` = "data frame of 1 row with columns left right top bottom."),  
     inside = list(definition = "inside <- function\n### Calculate for each box how many points are inside.\n(boxes,\n### Data frame of box descriptions, each row is 1 box, need columns\n### left right top bottom.\n points\n### Data frame of points, each row is 1 point, need columns x y.\n ){\n  sapply(1:nrow(boxes),function(i)in1box(points,boxes[i,]))\n### Vector of point counts for each box.\n}",
       format="",title="inside",
         description = "Calculate for each box how many points are inside.",  
         `item{boxes}` = "Data frame of box descriptions, each row is 1 box, need columns\nleft right top bottom.",  
         `item{points}` = "Data frame of points, each row is 1 point, need columns x y.",  
         value = "Vector of point counts for each box."), label.endpoints = list( 
         definition = "label.endpoints <- function\n### Make a Positioning Method that labels a certain x value.\n(FUN,\n### FUN(d$x) should return an index of which point to label. for\n### example you can use which.min or which.max.\n hjust\n### hjust of the labels.\n ){\n  function(d,...)ddply(d,.(groups),function(d,...){\n    i <- FUN(d$x)\n    if(length(i))data.frame(d[i,],hjust,vjust=0.5)\n    else data.frame()\n  })\n### A Positioning Method like first.points or last.points.\n}",
                                                            format="",
                                              title="label endpoints",
         description = "Make a Positioning Method that labels a certain x value.",  
         `item{FUN}` = "FUN(d$x) should return an index of which point to label. for\nexample you can use which.min or which.max.",  
         `item{hjust}` = "hjust of the labels.", value = "A Positioning Method like first.points or last.points."),  
     perpendicular.lines = list(definition = "perpendicular.lines <- function\n### Draw a line between the centers of each cluster, then draw a\n### perpendicular line for each cluster that goes through its\n### center. For each cluster, return the point the lies furthest out\n### along this line.\n(d,\n### Data frame with groups x y.\n debug=FALSE,\n### If TRUE will draw points at the center of each cluster and some\n### lines that show how the points returned were chosen.\n ...\n### ignored.\n ){\n  means <- rename(get.means(d),list(x=\"mx\",y=\"my\",groups=\"groups\"))\n  big <- merge(d,means,by=\"groups\")\n  fit <- lm(my~mx,means)\n  b <- coef(fit)[1]\n  m <- coef(fit)[2]\n  big2 <- transform(big,x1=(mx+x+(my-y)*m)/2)\n  big3 <- transform(big2,y1=m*(x1-x)+y)\n  big4 <- transform(big3,\n                    d=sqrt((x-x1)^2+(y-y1)^2),\n                    dm=sqrt((x-mx)^2+(y-my)^2))\n  big5 <- transform(big4,ratio=d/dm)\n  winners <- ddply(big5,.(groups),subset,\n                   subset=seq_along(ratio)==which.min(ratio))\n  ## gives back a function of a line that goes through the designated center\n  f <- function(v)function(x){\n    r <- means[means$groups==v,]\n    -1/m*(x-r$mx)+r$my\n  }\n  ##dd <- ddply(means,.(groups),summarise,x=x+sdx*seq(0,-2,l=5)[-1])\n  ##dd$y <- mdply(dd,function(groups,x)f(groups)(x))$x\n  if(debug){\n    ## myline draws a line over the range of the data for a given fun F\n    myline <- function(F)\n      grid.lines(range(d$x),F(range(d$x)),default.units=\"native\")\n    ## Then draw a line between these means\n    myline(function(x)m*x+b)\n    ## Then draw perpendiculars that go through each center\n    for(v in means$groups)myline(f(v))\n  }\n  winners[,c(\"x\",\"y\",\"groups\")]\n### Data frame with groups x y, giving the point for each cluster\n### which is the furthest out along the line drawn through its center.\n}",
       format="",title="perpendicular lines",
         description = "Draw a line between the centers of each cluster, then draw a\nperpendicular line for each cluster that goes through its\ncenter. For each cluster, return the point the lies furthest out\nalong this line.",  
         `item{d}` = "Data frame with groups x y.", `item{debug}` = "If TRUE will draw points at the center of each cluster and some\nlines that show how the points returned were chosen.",  
         `item{\\dots}` = "ignored.", value = "Data frame with groups x y, giving the point for each cluster\nwhich is the furthest out along the line drawn through its center."),  
     project.onto.segments = list(definition = "project.onto.segments <- function\n### Given a point and a set of line segments representing a convex or\n### alpha hull, calculate the closest point on the segments.\n(m,\n### m is 1 row, a center of a point cloud, we need to find the\n### distance to the closest point on each segment of the convex\n### hull.\n hull.segments,\n### Data frame describing the line segments of the convex or alpha\n### hull.\n debug=FALSE\n ){\n  these <- within(hull.segments,{\n    s <- (y2-y1)/(x2-x1)\n    ## the closest point on the line formed by expanding this line\n    ## segment (this expression is calculated by finding the minimum\n    ## of the distance function).\n    xstar <- (m$x + m$y*s + x1*s^2 - s*y1)/(s^2+1)\n    minval <- apply(cbind(x1,x2),1,min)\n    maxval <- apply(cbind(x1,x2),1,max)\n    ## xopt is the closest point on the line segment\n    xopt <- ifelse(xstar<minval,minval,ifelse(xstar>maxval,maxval,xstar))\n    yopt <- s*(xopt-x1)+y1\n    ## distance to each point on line segment from the center\n    d <- (m$x-xopt)^2+(m$y-yopt)^2\n  })\n  i <- which.min(these$d)\n  h <- with(these[i,],data.frame(x=xopt,y=yopt))\n  if(debug)with(h,lsegments(m$x,m$y,h$x,h$y))\n  h\n}",
       format="",title="project onto segments",
         description = "Given a point and a set of line segments representing a convex or\nalpha hull, calculate the closest point on the segments.",  
         `item{m}` = "m is 1 row, a center of a point cloud, we need to find the\ndistance to the closest point on each segment of the convex\nhull.",  
         `item{hull.segments}` = "Data frame describing the line segments of the convex or alpha\nhull."),  
     qp.labels = list(definition = "qp.labels <- function(var,spacer)function(d,...){\n  if(!spacer%in%names(d))stop(\"need to have calculated \",spacer)\n  require(quadprog)\n  d <- d[order(d[,var],decreasing=TRUE),]\n  ## sorts data so that m_1 is on top, m_n on bottom.\n  n <- nrow(d)\n  D <- diag(rep(1,n))\n  A <- diag(rep(1,n))[,-n]-rbind(0,diag(rep(1,n-1)))\n  h <- d[,spacer]\n  b0 <- (h[-n]+h[-1])/2\n  sol <- solve.QP(D,d[,var],A,b0)\n  d[,var] <- sol$solution\n  d\n}",
       format="",
       title="qp labels",
         description = "Use a QP solver to find the best places to put the points on a\nline, subject to the constraint that they should not overlap"),  
     smart.grid = list(definition = "smart.grid <- empty.grid.fun(big.boxes)",
       format="",
       title="smart grid",
         description = "Use bounding box information with a small empty.grid to find the a\nnon-colliding label that is close to a point on the convex hull,\nwhich is close to the visual center of the data. TODO: does not\nwork with ggplot2 since the backend does not support bounding box\ncalculation."),  
     vertical.qp = list(definition = "vertical.qp <- function(M)list(M,calc.boxes,qp.labels(\"y\",\"h\"))",
       format="",
       title="vertical qp",
         description = "Make a Positioning Method from a set of points on a vertical line\nthat will be spaced out using qp.labels"),  
     visualcenter = list(definition = "visualcenter <-\n  dl.indep(unique(transform(d,x=diff(range(x))/2+min(x),\n                            y=diff(range(y))/2+min(y))))",
       format="",title="visualcenter",
         description = "Point in the middle of the min and max for each group.")) 

### Geoms which need translation before applying Positioning Function.
need.trans.ggplot <- c("density")
direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  ##lvar <- if("group" %in% names(p$mapping))"group" else "colour"
  varnames <- c(groups="colour",x="x")
  if("y" %in% names(p$mapping))varnames <- c(varnames,y="y")
  rename.vec <- sapply(p$mapping[varnames],deparse)
  rename.vec <- gsub("factor[(]([^)]+)[)]","\\1",rename.vec)
  d <- structure(p$data[,rename.vec],names=names(varnames))
  geom <- p$layers[[1]]$geom$objname
  ldefault <- if(nlevels(p$data$colour)==2)"lines2" else "first.points"
  if(is.null(method))method <-
    switch(geom,
           density="top.points",
           line=ldefault,
           point="empty.grid.2",
           stop("No default label placement for this type of ggplot."))
  if(geom%in%need.trans.ggplot)method <-
    c(paste("trans.",geom,sep=""),method)
  dlgeom <- geom_text(position=position_dl(list(method),debug))
  ##print(dlgeom)
  p+dlgeom+opts(legend.position="none") ## maybe eventually create a scale?
### The ggplot object with direct labels added.
}
### Position class for direct label placement.
PositionDl <- proto(ggplot2::Position,{
  method <- NULL
  debug <- FALSE
  new <- function(., method=NULL, debug=FALSE) {
    .$proto(method=method,debug=debug)
  }
  adjust <- function(.,data,scales){
    ##print(data)
    ##browser()
    if(is.null(data$colour))stop("Need colour aesthetic to direct label.")
    labtab <- label.positions(x=data$x,y=data$y,groups=data$colour,
                              subscripts=1:nrow(data),method=.$method[[1]],
                              debug=.$debug)
    r <- transform(labtab,label=groups,group=groups,
                   colour=groups,
                   angle=rot)
    ##print(r)
    r
  }
  objname <- "dl"
})
### Position for internal use with geom_text.
position_dl <- PositionDl$build_accessor()


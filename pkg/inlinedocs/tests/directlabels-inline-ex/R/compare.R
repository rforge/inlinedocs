dlcompare <- function
### Compare several plots and/or label placement methods. This creates
### a custom grid graphics display based on lattice and/or ggplot2
### output. This is possible because the direct.label function is
### generic. The result will be a plot matrix with plots on the
### columns and positioning methods on the rows. Names from the lists
### will be used to annotate the plot. If label placement method list
### elements are not named, but are given as a character string
### designating a Positioning Function, we will use this name for the
### label.
(plots,
### List of ggplot2 or lattice plots.
 pos.funs
### List of label placement methods to apply to each plot.
 ){
  ## Augment positioning function list names if possible
  names(pos.funs) <- sapply(seq_along(pos.funs),function(i){
    N <- names(pos.funs)[i]
    f <- pos.funs[[i]]
    if(!is.null(N)&&N!="")N
    else if(class(f)=="character")f
    else ""
  })
  if(sum(names(pos.funs)!="")==0)names(pos.funs) <- NULL
  grid.newpage()
  rowadd <- if(is.null(names(plots)))0 else 1
  widths <- rep("null",l=length(plots))
  if(!is.null(names(pos.funs)))widths <- c(widths,"cm")
  heights <- rep("null",l=length(pos.funs))
  if(rowadd)heights <- c("cm",heights)
  the.layout <-
    grid.layout(length(heights),length(widths),
                widths=unit(1,widths),
                heights=unit(1,heights))
  pushViewport(viewport(layout=the.layout))
  for(col in seq_along(plots)){
    if(!is.null(names(plots))){
      pushViewport(viewport(layout.pos.col=col,layout.pos.row=1))
      grid.text(names(plots)[col])
      popViewport()
    }
    for(row in seq_along(pos.funs)){
      if(col==1&&!is.null(names(pos.funs))){
        pushViewport(viewport(layout.pos.col=length(plots)+1,
                              layout.pos.row=row+rowadd))
        grid.text(names(pos.funs)[row],rot=-90)
        popViewport()
      }
      pushViewport(viewport(layout.pos.col=col,layout.pos.row=row+rowadd))
      print(direct.label(plots[[col]],pos.funs[[row]]),newpage=FALSE)
      grid.rect()
      popViewport()
    }
  }
  popViewport()
  return()

  dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
  ddf <- melt(as.data.frame(dts),id="time")
  names(ddf) <- c("time","sex","deaths")
  plots <- list(lattice=
                xyplot(deaths~time,ddf,groups=sex,type="l",xlim=c(-15,80)),
                ggplot2=
                qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-10,80))
  pos.funs <- list("first.points","lines2")
  dlcompare(plots,pos.funs)
  ## Try some more exotic labeling options.
  exotic <- list(lines2,
                 rot=c(0,180),
                 fontsize=c(10,20),
                 fontface=c("bold","italic"),
                 fontfamily=c("mono","serif"),
                 alpha=c(0.25,1))
  ## Currently ggplot2 backend doesn't support face and family.
  dlcompare(plots,list(exotic))
  ## All of these subsets should produce valid comparison plots.
  dlcompare(plots[1],pos.funs[1])
  dlcompare(plots[1],pos.funs)
  dlcompare(plots,pos.funs[1])
  named.funs <- list(first.points=first.points,lines2=lines2)
  mixed.funs <- list("first.points",lines2=lines2,last.points)
  not.named <- structure(named.funs,names=NULL)
  unlabeled.plots <- structure(plots,names=NULL)
  dlcompare(plots,mixed.funs)
  dlcompare(plots,mixed.funs[3])

  data(BodyWeight,package="nlme")
  ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type="l",layout=c(3,1))
  ## lines2 works well only when there are 2 groups
  dlcompare(list(plots[[1]],ratplot),list("first.points","lines2"))
}

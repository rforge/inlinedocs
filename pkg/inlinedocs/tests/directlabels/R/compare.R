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
}

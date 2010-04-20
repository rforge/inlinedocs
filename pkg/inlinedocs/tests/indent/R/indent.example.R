indent.example <- function(
		### this function does nothing in particular and does it very well
	first         
		### the first argument
	,second
		### the second argument with comma
	,third 			##<< an argument with comma before and online comment
	,forth=        ##<< the second argument with a komma before and a list default value
		## and descriptions of each of the elements
		##describe<<
		list(this="that", ##<< whichness
			the="other", ##<< of the
			rhubarb="stew", ##<< why
			foo="bar")
		##end<<
	){
	##description<<why should I add to description?
	##details<<
	## if second is TRUE then first is returned
	invisible("")
	### invisible something not unrelated to first
	if ( second ){
		##alias<<Long silly alias
		res <- first
	} else {
		##details<<
		## if second is not TRUE then a list is returned
		##describe<<The contents of the list are:
		res <- list(x=7, ##<< x coordinate
			z= ##<< z describes everything else
				##describe<<
				list(colour=green, ##<< colour of line
					width=2),     ##<< width of line
			##end<<
			## and this line should get into documentation for z
			y=10)##<< y coordinate
	}
	##note<< a note
	##references<< a reference
	##seealso<< \code{\link{test}}
	##keyword<<documentation utilities
}


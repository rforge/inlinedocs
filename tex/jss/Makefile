jss762.pdf: jss762.tex jss762.bib apply.pdf
	pdflatex jss762.tex
	bibtex inlinedocs
	pdflatex jss762.tex
	pdflatex jss762.tex
apply.pdf: man/apply.Rd
	R CMD Rd2pdf man/apply.Rd
jss762.tex: jss762.Rnw doclist.R R/apply.R man/apply.Rd alternate.R
	PATH=~/bin:$(PATH) R CMD Sweave inlinedocs.Rnw
	sed -i 's/>>>//' jss762.tex
man/apply.Rd: R/apply.R DESCRIPTION
	R CMD INSTALL inlinedocs_1.9.1.tar.gz
	echo 'library(inlinedocs);package.skeleton.dx(".")'|R --vanilla
check: jss762.Rnw check.R
	R --no-save < check.R |tee check
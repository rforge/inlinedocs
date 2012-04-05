inlinedocs.pdf: inlinedocs.tex refs.bib 
	pdflatex inlinedocs.tex
	bibtex inlinedocs
	pdflatex inlinedocs.tex
	pdflatex inlinedocs.tex
inlinedocs.tex: inlinedocs.Rnw doclist.R R/apply.R man/apply.Rd alternate.R
	PATH=~/bin:$(PATH) R CMD Sweave inlinedocs.Rnw
	sed -i 's/>>>//' inlinedocs.tex
man/apply.Rd: R/apply.R DESCRIPTION
	echo 'library(inlinedocs);package.skeleton.dx(".")'|R --vanilla
check: inlinedocs.Rnw check.R
	R --no-save < check.R |tee check
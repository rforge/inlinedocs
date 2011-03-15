inlinedocs.pdf: inlinedocs.tex refs.bib
	pdflatex inlinedocs.tex
	bibtex inlinedocs
	pdflatex inlinedocs.tex
	pdflatex inlinedocs.tex
inlinedocs.tex: inlinedocs.Rnw doclist.R
	PATH=~/bin:$(PATH) R CMD Sweave inlinedocs.Rnw
	sed -i 's/>>>//' inlinedocs.tex
inlinedocs.check: inlinedocs.Rnw check.R
	R --no-save < check.R
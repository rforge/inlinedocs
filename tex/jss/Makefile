inlinedocs.pdf: inlinedocs.tex refs.bib
	pdflatex inlinedocs.tex && bibtex inlinedocs && pdflatex inlinedocs.tex && pdflatex inlinedocs.tex
inlinedocs.tex: inlinedocs.Rnw
	R CMD Sweave inlinedocs.Rnw
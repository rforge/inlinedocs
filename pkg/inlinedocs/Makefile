all: docs check
.PHONY: docs all
docs: R/package.skeleton.dx.R etc/make.R
	cd R && R --no-save < ../etc/make.R
check: 
	R CMD check .

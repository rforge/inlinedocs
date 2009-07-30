all: docs check
.PHONY: docs all check
docs: R/package.skeleton.dx.R etc/make.R
	cd R && R --no-save < ../etc/make.R
check: 
	cd .. && R CMD check inlinedocs

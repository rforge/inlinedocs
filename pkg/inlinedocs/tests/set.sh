#!/bin/bash
# Set unit tests output file to current output.
# This should only be run when we add unit tests or
# intentionally change the output of the package.
R CMD INSTALL ..
R --no-save 2>&1 <unit.tests.R|tee unit.tests.Rout.save 

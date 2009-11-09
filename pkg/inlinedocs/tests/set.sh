#!/bin/bash
R CMD INSTALL ..
R --no-save 2>&1 <unit.tests.R|tee unit.tests.Rout.save 

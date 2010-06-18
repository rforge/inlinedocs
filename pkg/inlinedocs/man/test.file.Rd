\name{test.file}
\alias{test.file}
\title{test file}
\description{Check an R code file with inlinedocs to see if the
extract.docs.file parser accurately extracts all the code inside!
The code file should contain a variable .result which is the
documentation list that you should get when you apply
extract.docs.file to the file. We check for identity of elements
of elements of the list, so the order of elements should not
matter, and thus this should be a good robust unit test.}
\usage{
test.file(f)
}
\arguments{
  \item{f}{File name of R code file with inlinedocs to parse and check.}
}



\author{Inlinedocs development team <inlinedocs-support@lists.r-forge.r-project.org>}





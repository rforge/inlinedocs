\name{extract.docs.file}
\alias{extract.docs.file}
\title{extract docs file}
\description{Apply all parsers relevant to extract info from just 1 code file.}
\usage{
extract.docs.file(f, parsers = NULL, ...)
}
\arguments{
  \item{f}{File name of R code to read and parse.}
  \item{parsers}{Parser Functions to use to parse the code and extract
documentation.}
  \item{\dots}{Other arguments to pass to Parser Functions.}
}



\author{Inlinedocs development team <inlinedocs-support@lists.r-forge.r-project.org>}




\examples{
f <- system.file("silly","R","silly.R",package="inlinedocs")
extract.docs.file(f)
}

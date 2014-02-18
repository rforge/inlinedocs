\name{extract.docs.file}
\alias{extract.docs.file}
\title{extract docs file}
\description{Apply all parsers relevant to extract info from just 1 code file.}
\usage{extract.docs.file(f, parsers = NULL, inlinedocs.exampleDir = file.path("..", 
    "..", "inst", "tests"), inlinedocs.exampleTrunk = "example.", 
    ...)}\arguments{
  \item{f}{File name of R code to read and parse.}
  \item{parsers}{Parser Functions to use to parse the code and extract
documentation.}
  \item{inlinedocs.exampleDir}{A string pointing to the location where inlinedocs should search for external examples}
  \item{inlinedocs.exampleTrunk}{A string used to identify the files containing external examples in the example directory. All file names of external examples have to start with this string}
  \item{\dots}{Other arguments to pass to Parser Functions.}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}




\examples{
f <- system.file("silly","R","silly.R",package="inlinedocs")
extract.docs.file(f)
}

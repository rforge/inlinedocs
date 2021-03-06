\name{apply.parsers}
\alias{apply.parsers}
\title{apply parsers}
\description{Parse code to r objs, then run all the parsers and return the
documentation list.}
\usage{apply.parsers(code, parsers = default.parsers, verbose = FALSE, 
    inlinedocs.exampleDir, inlinedocs.exampleTrunk, ...)}\arguments{
  \item{code}{Character vector of code lines.}
  \item{parsers}{List of Parser Functions.}
  \item{verbose}{Echo names of Parser Functions?}
  \item{inlinedocs.exampleDir}{A string pointing to the location where inlinedocs should search for external examples}
  \item{inlinedocs.exampleTrunk}{A string used to identify the files containing external examples in the example directory. All file names of external examples have to start with this string}
  \item{\dots}{Additional arguments to pass to Parser Functions.}
}

\value{A list of extracted documentation from code.}

\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}






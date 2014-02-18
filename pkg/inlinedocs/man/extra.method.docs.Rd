\name{extra.method.docs}
\alias{extra.method.docs}
\title{extra method docs}
\description{can be used in the parser list of package.skeleton.dx(). TODO:}
\usage{extra.method.docs(code, objs, env, inlinedocs.exampleDir, inlinedocs.exampleTrunk, 
    ...)}\arguments{
  \item{code}{Code lines in a character vector containing multiple R objects to
parse for documentation.}
  \item{objs}{The objects defined in the code.}
  \item{env}{The environment they inhibit (needed to pass on)}
  \item{inlinedocs.exampleDir}{A string pointing to the location where inlinedocs should search for external examples}
  \item{inlinedocs.exampleTrunk}{A string used to identify the files containing external examples in the example directory. All file names of external examples have to start with this string}
  \item{\dots}{ignored}
}

\value{named list of lists, one for each object to document.}

\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}






\name{extra.class.docs}
\alias{extra.class.docs}
\title{Extract documentation from code chunks}
\description{Parse R code to extract inline documentation from comments around
each class 
looking at the "source" attribute. This is a Parser Function that
can be used in the parser list of package.skeleton.dx(). TODO:}
\usage{extra.class.docs(code, objs, env, ...)}\arguments{
  \item{code}{Code lines in a character vector containing multiple R objects to
parse for documentation.}
  \item{objs}{The objects defined in the code.}
  \item{env}{The environment they inhibit (needed to pass on)}
  \item{\dots}{ignored}
}

\value{named list of lists, one for each object to document.}

\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}






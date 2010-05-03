\name{extract.docs.file}
\alias{extract.docs.file}
\title{Extract documentation from a file}
\description{Parse R code to extract inline documentation from comments around
each function. These are not able to be retreived simply by
looking at the "source" attribute. This is a Parser Function that
can be used in the parser list of package.skeleton.dx().}
\usage{
extract.docs.file(code, objs, ...)
}
\arguments{
  \item{code}{Code lines in a character vector containing multiple R objects to
parse for documentation.}
  \item{objs}{The objects defined in the code.}
  \item{\dots}{ignored}
}

\value{named list of lists, one for each object to document.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>, Keith Ponting <k.ponting@aurix.com>}






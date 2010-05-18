\name{extract.docs.code}
\alias{extract.docs.code}
\title{extract docs code}
\description{Write code to a file and parse it to r objs, then run all the
parsers and return the documentation list.}
\usage{
extract.docs.code(code, parsers, verbose = TRUE, ...)
}
\arguments{
  \item{code}{Character vector of code lines.}
  \item{parsers}{List of Parser Functions.}
  \item{verbose}{Echo names of Parser Functions?}
  \item{\dots}{Additional arguments to pass to Parser Functions.}
}

\value{A list of extracted documentation from code.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>, Keith Ponting <k.ponting@aurix.com>}






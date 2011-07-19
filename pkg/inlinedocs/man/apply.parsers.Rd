\name{apply.parsers}
\alias{apply.parsers}
\title{apply parsers}
\description{Parse code to r objs, then run all the parsers and return the
documentation list.}
\usage{
apply.parsers(code,
parsers = default.parsers,
verbose = FALSE,
...)
}
\arguments{
  \item{code}{Character vector of code lines.}
  \item{parsers}{List of Parser Functions.}
  \item{verbose}{Echo names of Parser Functions?}
  \item{\dots}{Additional arguments to pass to Parser Functions.}
}

\value{A list of extracted documentation from code.}

\author{Inlinedocs development team
<inlinedocs-support@lists.r-forge.r-project.org>}






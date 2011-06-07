\name{forall}
\alias{forall}
\title{forall}
\description{For each object in the package that satisfies the criterion
checked by subfun, parse source using FUN and return the resulting
documentation list.}
\usage{
forall(FUN, subfun = function(x) TRUE)
}
\arguments{
  \item{FUN}{Function to apply to each element in the package.}
  \item{subfun}{Function to select subsets of elements of the package, such as
is.function. subfun(x)==TRUE means FUN will be applied to x and
the result will be returned.}
}

\value{A Parser Function.}

\author{Inlinedocs development team
<inlinedocs-support@lists.r-forge.r-project.org>}






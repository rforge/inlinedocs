\name{extract.docs.fun}
\alias{extract.docs.fun}
\title{Extract documentation from a function}
\description{Given a function, return a list describing inline documentation in
the source of that function (relies on source attr).}
\usage{
extract.docs.fun(fun, name.fun)
}
\arguments{
  \item{fun}{The function to examine.}
  \item{name.fun}{The name of the function for use in warning messages.}
}

\value{as for \code{\link{extract.docs.chunk}}}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>, Keith Ponting <k.ponting@aurix.com>}






\name{extract.docs.fun}
\Rdversion{1.1}
\alias{extract.docs.fun}
\title{ Extract documentation from a function}
\description{Given a function, return a list describing inline documentation in the source of that function (relies on source attr).}
\usage{
extract.docs.fun(fun)
}
\arguments{
  \item{fun}{The function to examine.}
}
\details{
}
\value{Named list of character strings extracted from comments. For each name N we will look for N\{...\} in the Rd file and replace it with the string in this list (implemented in modify.Rd.file).}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>}
\note{
}


\seealso{
}
\examples{}
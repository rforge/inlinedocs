\name{extract.docs.file}
\Rdversion{1.1}
\alias{extract.docs.file}
\title{Extract documentation from a file}
\description{Parse an R code file and extract inline documentation from
comments around each function.}
\usage{
extract.docs.file(code.file, write.examples = TRUE)
}
\arguments{
  \item{code.file}{The R code file to parse.}
  \item{write.examples}{Gather examples from test files?}
}
\details{
}
\value{named list of lists. Each element is the result of a call to
extract.docs.fun, with names corresponding to functions found in
the R code file.}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>, Keith Ponting <k.ponting@aurix.com>}
\note{
}


\seealso{
}
\examples{}

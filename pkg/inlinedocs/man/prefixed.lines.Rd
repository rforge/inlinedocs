\name{prefixed.lines}
\alias{prefixed.lines}
\title{prefixed lines}
\description{The primary mechanism of inline documentation is via consecutive
groups of lines matching the specified prefix regular expression
"" (i.e. lines beginning with "") are
collected as follows into documentation sections:\describe{
\item{description}{group starting at line 2 in the code}
\item{arguments}{group following each function argument}
\item{value}{group ending at the penultimate line of the code}}
These may be added to by use of the  constructs
described below.}
\usage{
prefixed.lines(src, ...)
}
\arguments{
  \item{src}{
}
  \item{\dots}{
}
}



\author{Inlinedocs development team
<inlinedocs-support@lists.r-forge.r-project.org>}




\examples{
test <- function
### the description
(x,
### the first argument
 y ##<< another argument
 ){
  5
### the return value
##seealso<< foobar
}
src <- attr(test,"source")
prefixed.lines(src)
extract.xxx.chunks(src)
}

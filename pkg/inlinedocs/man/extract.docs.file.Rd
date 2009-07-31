\name{extract.docs.file}
\Rdversion{1.1}
\alias{extract.docs.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
extract docs file
}
\description{
Parse an R code file and extract inline documentation from comments around each function.
}
\usage{
extract.docs.file(code.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{code.file}{
The R code file to parse.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
named list of lists. Each element is the result of a call to extract.docs.fun, with names corresponding to functions found in the R code file.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Toby Dylan Hocking <toby.hocking@etu.upmc.fr>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

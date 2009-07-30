\name{package.skeleton.dx}
\Rdversion{1.1}
\alias{package.skeleton.dx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
package skeleton dx
}
\description{
Automates more of the setup process for a new source package. After calling package.skeleton, it inspects the specified R code files to find inline documentation, which it adds to the Rd files. Should be called when the working directory is a packagename/R directory. It will overwrite files in the packagename/man directory.
}
\usage{
package.skeleton.dx(name = gsub(".*/([^/]*)/R", "\\\\1", getwd(), fixed = FALSE), code_files = grep(".*\\\\.R$", dir(), val = TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
the package name and directory name for your package, to be passed to package.skeleton
}
  \item{code_files}{
character vector with the paths to the R code files, to be passed to package.skeleton, and also inspected for inline documentation
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\name{package.skeleton.dx}
\Rdversion{1.1}
\alias{package.skeleton.dx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
package skeleton dx
}
\description{
Automates more of the setup process for a new source package. After inspecting the specified R code files to find inline documentation, it calls the standard package.skeleton function, which creates bare Rd files. The inline documentation is added to these Rd files and then these files are copied to ../man. Should be called when the working directory is a packagename/R directory. It will overwrite files in the packagename/man directory.
}
\usage{
package.skeleton.dx(code_files = Sys.glob("*.R"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{code_files}{
character vector with the paths to the R code files in the current working directory, to be passed to package.skeleton, and also inspected for inline documentation
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
Toby Dylan Hocking <toby.hocking@etu.upmc.fr>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

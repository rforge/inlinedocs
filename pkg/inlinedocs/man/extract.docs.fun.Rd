\name{extract.docs.fun}
\Rdversion{1.1}
\alias{extract.docs.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
extract docs fun
}
\description{
Given a function name, return a list describing inline documentation in the source of that function.
}
\usage{
extract.docs.fun(fun)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
The function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Named list of character strings extracted from comments. Each name corresponds to an argument of the function or a special keyword (recognized by name.to.match.string) used to place the comments in the Rd file.
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

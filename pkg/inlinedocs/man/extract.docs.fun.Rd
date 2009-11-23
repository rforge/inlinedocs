\name{extract.docs.fun}
\Rdversion{1.1}
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
\details{The primary mechanism is that consecutive groups of lines matching
the specified prefix regular expression "\code{^### }" (i.e. lines
beginning with "\code{### }") are collected
as follows into documentation sections:\describe{
\item{description}{group starting at line 2 in the code}
\item{arguments}{group following each function argument}
\item{value}{group ending at the penultimate line of the code}}
These may be added to by use of the \code{##<<} constructs described below.

For simple functions/arguments, the argument may also be
documented by appending \code{##<<} comments on the same line as the
argument name. Mixing this mechanism with \code{###} comment lines for
the same argument is likely to lead to confusion, as the \code{###}
lines are processed first.

Additionally, consecutive sections of \code{##} comment
lines beginning with \code{##}\emph{xxx}\code{<<} (where
\emph{xxx} is one of the fields: \code{alias}, \code{details},
\code{keyword}, \code{references}, \code{author}, \code{note},
\code{seealso}, \code{value}, \code{title} or \code{description})
are accumulated and inserted in the relevant part of the .Rd
file.

For \code{value}, \code{title}, \code{description} and function
arguments, these \emph{append} to any text from "prefix"
(\code{^### }) comment lines, irrespective of the order in the
source.

Each separate extra section appears as a new
paragraph except that: \itemize{\item empty sections (no
matter how many lines) are ignored;\item \code{alias} and
\code{keyword} sections have special rules;\item
\code{description} should be brief, so all such sections
are concatenated as one paragraph;\item \code{title} should
be one line, so any extra \code{title} sections are
concatenated as a single line with spaces separating the
sections.}

As a special case, the construct \code{##describe<<} causes
similar processing to the main function arguments to be
applied in order to construct a describe block within the
documentation, for example to describe the members of a
list. All subsequent "same line" \code{##<<} comments go into that
block until terminated by a subsequent \code{##}\emph{xxx}\code{<<} line.


Such regions may be nested, but not in such a way
that the first element in a \code{describe} is another \code{describe}.
Thus there must be at least one \code{##<<} comment between each
pair of \code{##describe<<} comments.

When nested \code{describe} blocks are used, a comment-only
line with \code{##end<<} terminates the current level only; any
other valid \code{##}\emph{xxx}\code{<<} line terminates
all open describe blocks.}
\value{Named list of character strings extracted from comments. For each
name N we will look for N\{...\} in the Rd file and replace it
with the string in this list (implemented in modify.Rd.file).}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>, Keith Ponting <k.ponting@aurix.com>}
\note{\code{alias} extras are automatically split at new lines.

\code{keyword} extras are automatically split at white space,
as all the valid keywords are single words.}


\seealso{
}
\examples{}

\keyword{documentation}
\keyword{utilities}

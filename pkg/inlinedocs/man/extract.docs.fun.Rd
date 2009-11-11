\name{extract.docs.fun}
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
For simple functions/arguments, the argument may also be documented by
appending ##<< comments on the same line as the argument. For those who
wish to become confused, any following ### comment lines will be appended.


Additionally, contiguous sections of ## comment lines beginning with
<<xxx (where xxx is one of the "other" fields: alias, details, keyword,
references, author, note, seealso or value) are accumulated and inserted in
the relevant part of the .Rd file.

In the case of value, the extra information is appended to that from
any final ### comment lines.

Each separate extra section appears as a new paragraph
except that empty sections (no matter how many lines) are ignored.}
\value{Named list of character strings extracted from comments. For each name N we will look for N\{...\} in the Rd file and replace it with the string in this list (implemented in modify.Rd.file).}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>, Keith Ponting <k.ponting@aurix.com>}
\note{Alias extras are automatically split at new lines.

Keyword extras are auto-split at white space.}


\seealso{
}
\examples{}
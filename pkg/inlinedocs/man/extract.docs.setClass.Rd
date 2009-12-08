\name{extract.docs.setClass}
\alias{extract.docs.setClass}
\title{S4 class inline documentation}
\description{Using the same conventions as for functions, definitions of S4 classes
in the form \code{setClass("classname",\dots)} are also located and
scanned for inline comments.}
\usage{
extract.docs.setClass(code.file)
}
\arguments{
  \item{code.file}{
}
}
\details{Extraction of S4 class documentation is currently limited to expressions
within the source code which have first line starting with
\code{setClass("classname"}. These are located from the source file
(allowing also for white space around the \code{setClass} and \code{(}).
Note that \code{"classname"} must be a quoted character string;
expressions returning such a string are not matched.

For class definitions, the slots (elements of the
\code{representation} list) fill the role of function
arguments, so may be documented by \code{##<<} comments on
the same line or \code{### } comments at the beginning of the
following line.

The class definition skeleton includes an \code{Objects from the Class}
section, to which any \code{##details<<} documentation chunks are
written. It is given a vanilla content if there are no specific
\code{##details<<} documentation chunks.}
\value{
}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@inria.fr>, Keith Ponting <k.ponting@aurix.com>}
\note{
}


\seealso{
}
\examples{}

\name{package.skeleton.dx}
\alias{package.skeleton.dx}
\title{Package skeleton deluxe}
\description{Automates more of the setup process for a new source
package. After inspecting the specified R code files to find
inline documentation, it calls the standard package.skeleton
function, which creates bare Rd files. The inline documentation is
added to these Rd files and then these files are copied to
../man. It will overwrite files in the pkgdir/man directory.}
\usage{
package.skeleton.dx(pkgdir = "..", parsers = NULL, ...)
}
\arguments{
  \item{pkgdir}{package directory where the DESCRIPTION file lives. Your code
should be in pkgdir/R. We will setwd to pkgdir/R for the duration
of the function, then switch back to where you were previously.}
  \item{parsers}{List of Parser functions, which will be applied in sequence to
extract documentation from your code. Default NULL means to first
search for a definition in the variable "parsers" in
pkgdir/R/.inlinedocs.R, if that file exists. If not, we use the
list defined in options("inlinedocs.parsers"), if that is
defined. If not, we use the package global default in the variable
default.parsers.}
  \item{\dots}{Parameters to pass to Parser functions.}
}



\author{Toby Dylan Hocking <toby.hocking@inria.fr>, Keith Ponting <k.ponting@aurix.com>}






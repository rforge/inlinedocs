\name{package.skeleton.dx}
\alias{package.skeleton.dx}
\title{ Package skeleton deluxe}
\description{Automates more of the setup process for a new source
package. After inspecting the specified R code files to find
inline documentation, it calls the standard package.skeleton
function, which creates bare Rd files. The inline documentation is
added to these Rd files and then these files are copied to
../man. It will overwrite files in the pkgdir/man directory.}
\usage{
package.skeleton.dx(pkgdir = "..", code_files = NULL, check = "")
}
\arguments{
  \item{pkgdir}{package directory where the DESCRIPTION file lives. Your code
should be in pkgdir/R. We will setwd to pkgdir/R for the duration
of the function, then switch back to where you were previously.}
  \item{code_files}{Character vector with the names of the R code files, to be passed
to package.skeleton, and also inspected for inline
documentation. NULL means all files ending in .R.}
  \item{check}{Value indicating whether or not to check the package after
documentation is built. Anything other than "" (the default) will
check the package. "noex" means check the package without running
examples, only tests (useful for debugging if your tests/examples
take a long time).}
}
\details{
}
\value{
}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>, Keith Ponting <k.ponting@aurix.com>}
\note{
}


\seealso{
}
\examples{}
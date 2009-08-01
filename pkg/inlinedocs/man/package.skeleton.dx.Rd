\name{package.skeleton.dx}
\Rdversion{1.1}
\alias{package.skeleton.dx}
\title{package.skeleton.dx}
\description{Automates more of the setup process for a new source package. After inspecting the specified R code files to find inline documentation, it calls the standard package.skeleton function, which creates bare Rd files. The inline documentation is added to these Rd files and then these files are copied to ../man. It will overwrite files in the packagename/man directory.}
\usage{
package.skeleton.dx(chdir = ".", code_files = NULL, check = FALSE)
}
\arguments{
  \item{chdir}{packagename/R directory where your code lives. We will switch to this directory for the duration of the function, then switch back to where you were previously.}
  \item{code_files}{Character vector with the names of the R code files, to be passed to package.skeleton, and also inspected for inline documentation. NULL means all files ending in .R.}
  \item{check}{Logical indicating whether or not to check the package after documentation is built.}
}
\details{
}
\value{
}
\references{
}
\author{Toby Dylan Hocking <toby.hocking@etu.upmc.fr>}
\note{
}


\seealso{
}
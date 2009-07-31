\name{package.skeleton.dx}
\Rdversion{1.1}
\alias{package.skeleton.dx}
\title{package.skeleton.dx}
\description{Automates more of the setup process for a new source package. After inspecting the specified R code files to find inline documentation, it calls the standard package.skeleton function, which creates bare Rd files. The inline documentation is added to these Rd files and then these files are copied to ../man. Should be called when the working directory is a packagename/R directory. It will overwrite files in the packagename/man directory.}
\usage{
package.skeleton.dx(code_files = Sys.glob("*.R"))
}
\arguments{
  \item{code_files}{character vector with the paths to the R code files in the current working directory, to be passed to package.skeleton, and also inspected for inline documentation}
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
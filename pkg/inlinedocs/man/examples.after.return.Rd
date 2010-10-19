\name{examples.after.return}
\alias{examples.after.return}
\title{examples after return}
\description{Get examples from inline definitions after return()
PhG: this does not work well! Think of these situations:
1) You have multiple return() in the code of your function,
2) You have return() appearing is some example code, ...
I can hardly propose a hack here. The whole code of the function
must be parsed, and one must determine which one is the last line
of code that is actually executed.}
\usage{
examples.after.return(src, name = "", ...)
}
\arguments{
  \item{src}{
}
  \item{name}{
}
  \item{\dots}{
}
}



\author{Inlinedocs development team <inlinedocs-support@lists.r-forge.r-project.org>}






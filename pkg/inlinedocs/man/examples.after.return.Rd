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

\value{I make two propositions here
1) to keep the same mechanism that has the advantage of simplicity
   but to use a special tag}

\author{Inlinedocs development team <inlinedocs-support@lists.r-forge.r-project.org>}




\examples{
###    function code from examples explicitly, and
### 2) to place the example in an "ex" attribute
###    attached to the function
###    (see next parser). That solution will be also interesting for
###    documenting datasets, something not done yet by inlinedocs!
  ## Look for the examples mark
  m <- grep("^\\W*(##examples<<|#\\{\\{\\{examples)", src)
  if (!length(m)) return(list())
  if (length(m) > 1)
    warning("More than one examples tag for ", name,
            ". Taking the last one")
  m <- m[length(m)]
  ## Look for the lines containing return value comments just before
  r <- grep("\\s*### ", src[1:(m-1)])
    if (!length(r)) {
      value <- NULL
    } else {
      ## Only take consecutive lines before the mark
      keep <- rev((m - rev(r)) == 1:length(r))
      if (!any(keep)) {
        value <- NULL
      } else {
        value <- decomment(src[r[keep]])
      }
    }
  ## Collect now the example code beneath the mark
  ex <- src[(m + 1):(length(src) - 1)]
  ## Eliminate leading tabulations or four spaces
  prefixes <- gsub("(\\s*).*","\\1",ex,perl=TRUE)[grep("\\w",ex)]
  FIND <- prefixes[which.min(nchar(prefixes))]
  ex <- sub(FIND,"",ex)
  ## Add an empty line before and after example
  ex <- c("", ex, "")
  ## Return examples and value
  list(examples = paste(ex, collapse = "\n"), value = value)
}

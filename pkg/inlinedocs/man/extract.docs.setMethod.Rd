\name{extract.docs.setMethod}
\alias{extract.docs.setMethod}
\title{S4 mehtod inline documentation}
\description{Using the same conventions as for functions, definitions of S4 methods
in the form \code{setMethod(\dots)} are also located and
scanned for inline comments.}
\usage{extract.docs.setMethod(doc.link, env, inlinedocs.exampleDir, 
    inlinedocs.exampleTrunk)}\arguments{
  \item{doc.link}{DocLink object as created by \code{extract.file.parse}.}
  \item{env}{environment to find method source}
  \item{inlinedocs.exampleDir}{A string pointing to the location where inlinedocs should search for external examples}
  \item{inlinedocs.exampleTrunk}{A regular expression used to identify the files containing external examples in the example directory}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}






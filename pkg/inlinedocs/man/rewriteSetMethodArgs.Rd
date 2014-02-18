\name{rewriteSetMethodArgs}
\alias{rewriteSetMethodArgs}
\title{rewriteSetMethodArgs}
\description{Since we do not know if the arguments in the call to setMethod are given with
keywords, partially matching keywords as an ordered list or any 
combination of it, we use the same function as R  (match.arg ) 
to rewrite our argumentlist to a (pair)list from which
we can extract the information easily}
\usage{rewriteSetMethodArgs(lang)}\arguments{
  \item{lang}{
}
}



\author{Toby Dylan Hocking <toby@sg.cs.titech.ac.jp> [aut, cre], Keith Ponting [aut], Thomas Wutzler [aut], Philippe Grosjean [aut], Markus Müller [aut], R Core Team [ctb, cph]}






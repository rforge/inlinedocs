simple <- function (src, ...) {# a simple Parser Function
  #item{src} character vector of R source code.
  noquotes <- gsub("([\"'`]).*\\1", "", src)
  comments <- grep("#", noquotes, value = TRUE)
  doc.pattern <- "[^#]*#([^ ]*) (.*)"
  tags <- gsub(doc.pattern, "\\1", comments)
  docs <- as.list( gsub(doc.pattern, "\\2", comments) )
  names(docs) <- tags
  #value all the tags with a single pound sign.
  docs[ tags != "" ]
}

library(tools)
parsed <- parse_Rd("../man/package.skeleton.dx.Rd")
tags <- tools:::Rdtags
parsed[tools:::RdTags(parsed)=="TEXT"]=="\n"

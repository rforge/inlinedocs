#
# vim:set ff=unix expandtab ts=2 sw=2:
test.forfun=function(){
    code='  
    setGeneric(
    name="GenericFunc",
    where=e,
    def=function(# A short description of the generic function
      ### This function is generic
      .Object, ##<< d2 
      num 
      ){standardGeneric("GenericFunc")
      ##value<< d3
      }
    )
    '
    te=topenv(matchThisEnv =NULL )
    e=new.env()
    exprs <- parse(text=code,keep.source=TRUE)
    for (i in exprs){
          eval(i, env=e)
    }
}

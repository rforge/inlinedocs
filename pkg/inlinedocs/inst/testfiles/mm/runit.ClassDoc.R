#
# vim:set ff=unix expandtab ts=2 sw=2:
############################################################################################################################
test.extract.docs.setClass=function(){
    require("stringr")
    code='  
#################################################
setClass(#ExposedClass
   ### The class description
         ##details<< Put what you like in documentation details,
         ## but ideally reference construction methods.

   Class="ExposedClass",
   representation=representation(
        times="numeric" ##<< a dummy 
   )
)

#################################################
# overload the [] operator
  #define a hidden helper function (not in the NAMESPACE file   
  getSingleCol=function(x,slot_name){
      res=""
      if(slot_name=="times"){ res=exposedGeneric(x)}
      return(res)
  }
  #define the actual method
  setMethod("[",signature(x="ExposedClass",i="character"), #since [] is a already defined generic the names of the arguments are not arbitrary 
  definition=function(
  x, ##<< an object of  ExposedClass
  i  ##<< something
  ){
      n=length(i)
      df=getSingleCol(x,i[1])
      if (n>1){
          for (k in 2:n){
              df=cbind(df,getSingleCol(x,i[k]))
          }
      }
      return(df)
  }
)
#################################################
# overload the $ operator
setMethod("$",signature(x="ExposedClass"), #since $ is a already defined generic the names of the arguments are not arbitrary 
        definition=function(x,name){
            return(getSingleCol(x,name))
        }
)
#################################################
setGeneric(
    name="exposedGeneric",
    def=function( # convert its argument to a Delta14C representation
    ### this can be a number a matrix or an object of class FcAtm
    object ##<< an object that contains data and a formatdescription.  So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
    ){
        standardGeneric("exposedGeneric")
    }
)
#################################################
setMethod(
   f= "exposedGeneric",
   signature="ExposedClass",
   definition=function(object){
       return(object@times)
     }
)
#################################################
'
  fn="source.R"
  writeLines(code,con=fn)
	#
  parsers=NULL
  l=createObjects(code)
  objs=l[["objs"]] 
  e=l[["env"]] 
  l= extract.file.parse(code,e)

  dl=l[["ExposedClass"]]
  S4class.docs <- extract.docs.setClass(dl)
  res=S4class.docs#[["ExposedClass"]]
  pp("res",environment())
	checkEquals(res[["description"]],"The class description")
	checkEquals(str_trim(res[["details"]]),"Put what you like in documentation details,\nbut ideally reference construction methods.")
	checkEquals(str_trim(res[["section{Objects from the Class}"]]),"Put what you like in documentation details,\nbut ideally reference construction methods.")
	checkEquals(str_trim(res[["item{times}"]]),"a dummy")
	checkEquals(str_trim(res[["alias"]]),"ExposedClass")
	checkEquals(str_trim(res[["title"]][["title"]]),"ExposedClass") #funny behavior
  
  
  doc1=extra.class.docs(code,objs,e)
  doc2=forall.parsers$title.from.name("ExposedClass",doc1)
  
  
}

myFun <- structure(

function # My function
  ### Fun. description
(
  x ##<< an argument
)
{
  x * 2
},

ex=function()
{
  myFun(x)
})

.result <- 
 list(myFun = list(definition = "myFun <- structure(\n\nfunction # My function\n  ### Fun. description\n(\n  x ##<< an argument\n)\n{\n  x * 2\n},\n\nex=function()\n{\n  myFun(x)\n})",  
     description = "Fun. description", `item{x}` = "an argument",  
     title = "My function", format = "", examples = "\nmyFun(x)\n")) 

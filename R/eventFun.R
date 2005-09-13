##############
# create a class for links between callback functions and xevents
# callFun is the name of the callback function (character string)
# shortName is the function description
# preprocessFun is a character vector of all preprocessing functions that 
#  must be called; the vector should be function names that will be called 
#  using do.call  
#
# a few notes:
#  callFun must be a function that takes one parameter - an object of 
#   class AgNode (if the model is a graph) or a point (if the model is a 
#   data frame)
#  preprocessing functions must not take any parameters
##############
# there may be no preprocessing functions
setClassUnion("characterNULL", members=c("character", "NULL"))

setClass("gEventFun", representation(callFun="character", 
                                  shortName="character", 
                                  preprocessFun="characterNULL"))

if (is.null(getGeneric("callFun")))
  setGeneric("callFun", function(object)
            standardGeneric("callFun"))
setMethod("callFun", "gEventFun", function(object)
         object@callFun)

if (is.null(getGeneric("callFun<-")))
  setGeneric("callFun<-", function(object,value)
            standardGeneric("callFun<-"))
setReplaceMethod("callFun", "gEventFun", function(object,value)
         {
           object@callFun<-value
           object
         }
)

if (is.null(getGeneric("shortName")))
  setGeneric("shortName", function(object)
            standardGeneric("shortName"))
setMethod("shortName", "gEventFun", function(object)
         object@shortName)

if (is.null(getGeneric("shortName<-")))
  setGeneric("shortName<-", function(object,value)
            standardGeneric("shortName<-"))
setReplaceMethod("shortName", "gEventFun", function(object,value)
         {
           object@shortName<-value
           object
         }
)

if (is.null(getGeneric("preprocessFun")))
  setGeneric("preprocessFun", function(object)
            standardGeneric("preprocessFun"))
setMethod("preprocessFun", "gEventFun", function(object)
         object@preprocessFun)

if (is.null(getGeneric("preprocessFun<-")))
  setGeneric("preprocessFun<-", function(object,value)
            standardGeneric("preprocessFun<-"))
setReplaceMethod("preprocessFun", "gEventFun", function(object,value)
         {
           object@preprocessFun<-value
           object
         }
)



#############
# create a class for MVC objects
#############

#setClass("MVC", representation(model="gModel", viewList="list", 
#                  controller="environment", parentMVC="MVCNULL", 
#                  childMVCList="list"))
setClass("MVC", representation(model="gModel", viewList="list", 
                  controller="environment", parentMVC="character", 
                  childMVCList="list"))
setClassUnion("MVCNULL", members=c("MVC", "NULL"))

########
# get the slots
######## 

if (is.null(getGeneric("model")))
  setGeneric("model", function(object)
            standardGeneric("model"))
setMethod("model", "MVC", function(object)
         object@model)

if (is.null(getGeneric("viewList")))
  setGeneric("viewList", function(object)
            standardGeneric("viewList"))
setMethod("viewList", "MVC", function(object)
         object@viewList)

if (is.null(getGeneric("controller")))
  setGeneric("controller", function(object)
            standardGeneric("controller"))
setMethod("controller", "MVC", function(object)
         object@controller)

if (is.null(getGeneric("parentMVC")))
  setGeneric("parentMVC", function(object)
            standardGeneric("parentMVC"))
setMethod("parentMVC", "MVC", function(object)
         object@parentMVC)

if (is.null(getGeneric("childMVCList")))
  setGeneric("childMVCList", function(object)
            standardGeneric("childMVCList"))
setMethod("childMVCList", "MVC", function(object)
         object@childMVCList)

########
# set the slots
########

if (is.null(getGeneric("model<-")))
  setGeneric("model<-", function(object,value)
            standardGeneric("model<-"))
setReplaceMethod("model", "MVC", function(object,value)
         {
           object@model<-value
           object
         }
)

if (is.null(getGeneric("viewList<-")))
  setGeneric("viewList<-", function(object,value)
            standardGeneric("viewList<-"))
setReplaceMethod("viewList", "MVC", function(object,value)
         {
           object@viewList<-value
           object
         }
)

if (is.null(getGeneric("controller<-")))
  setGeneric("controller<-", function(object,value)
            standardGeneric("controller<-"))
setReplaceMethod("controller", "MVC", function(object,value)
         {
           object@controller<-value
           object
         }
)

if (is.null(getGeneric("parentMVC<-")))
  setGeneric("parentMVC<-", function(object,value)
            standardGeneric("parentMVC<-"))
setReplaceMethod("parentMVC", "MVC", function(object,value)
         {
           object@parentMVC<-value
           object
         }
)

if (is.null(getGeneric("childMVCList<-")))
  setGeneric("childMVCList<-", function(object,value)
            standardGeneric("childMVCList<-"))
setReplaceMethod("childMVCList", "MVC", function(object,value)
         {
           object@childMVCList<-value
           object
         }
)



setOldClass("GtkWindow")
setOldClass("GtkDrawingArea")
setOldClass("GtkCList")


##############
# create a class for model objects
##############
# a virtual model class that all model classes will be derived from
# modelData will be the actual model data, linkData will be any data
# that links the model to its parent model (a list of 2 functions:
# toParent and fromParent), virtualData is any data that
# is needed for views of the model, and modelName is the name of the model
# (a way for the user to refer to the data), modelVar are any variables that
# can be calculated from the modelData (or that refer to members of the
# modelData slot), but do not belong with the modelData
setClass("gModel", representation(modelData="ANY", linkData="list", 
         virtualData="ANY", modelName="character", modelVar="list"), 
         contains=("VIRTUAL"))

## for a model that has graph data
#setClass("graphModel", representation(modelData="graph"), contains="gModel")

## for a model that has expression data - 
## the data list should include an exprSet object and maybe a vector of LL ids
#setClass("exprModel", representation(modelData="exprSet"), contains="gModel")

# for a model that has data frame data
setClass("dfModel", representation(modelData="data.frame", 
                    virtualData="data.frame"), contains="gModel")

########
# methods
########
if (is.null(getGeneric("modelData")))
  setGeneric("modelData", function(object)
            standardGeneric("modelData"))
setMethod("modelData", "gModel", function(object)
         object@modelData)

if (is.null(getGeneric("modelData<-")))
  setGeneric("modelData<-", function(object,value)
            standardGeneric("modelData<-"))
setReplaceMethod("modelData", "gModel", function(object,value)
         {
           object@modelData<-value
           object
         }
)

if (is.null(getGeneric("modelName")))
  setGeneric("modelName", function(object)
            standardGeneric("modelName"))
setMethod("modelName", "gModel", function(object)
         object@modelName)

if (is.null(getGeneric("modelName<-")))
  setGeneric("modelName<-", function(object,value)
            standardGeneric("modelName<-"))
setReplaceMethod("modelName", "gModel", function(object,value)
         {
           object@modelName<-value
           object
         }
)

if (is.null(getGeneric("linkData")))
  setGeneric("linkData", function(object)
            standardGeneric("linkData"))
setMethod("linkData", "gModel", function(object)
         object@linkData)

if (is.null(getGeneric("linkData<-")))
  setGeneric("linkData<-", function(object,value)
            standardGeneric("linkData<-"))
setReplaceMethod("linkData", "gModel", function(object,value)
         {
           object@linkData<-value
           object
         }
)

if (is.null(getGeneric("virtualData")))
  setGeneric("virtualData", function(object)
            standardGeneric("virtualData"))
setMethod("virtualData", "gModel", function(object)
         object@virtualData)

if (is.null(getGeneric("virtualData<-")))
  setGeneric("virtualData<-", function(object,value)
            standardGeneric("virtualData<-"))
setReplaceMethod("virtualData", "gModel", function(object,value)
         {
           object@virtualData<-value
           object
         }
)

if (is.null(getGeneric("modelVar")))
  setGeneric("modelVar", function(object)
            standardGeneric("modelVar"))
setMethod("modelVar", "gModel", function(object)
         object@modelVar)

if (is.null(getGeneric("modelVar<-")))
  setGeneric("modelVar<-", function(object, value)
            standardGeneric("modelVar<-"))
setReplaceMethod("modelVar", "gModel", function(object, value)
         {
           object@modelVar<-value
           object
         }
)

##########
# create a method to update a model
##########
if (is.null(getGeneric("updateModel")))
  setGeneric("updateModel", function(object, type, data)
            standardGeneric("updateModel"))

if (is.null(getGeneric("provideInfo")))
  setGeneric("provideInfo", function(object, type, data)
            standardGeneric("provideInfo"))


###########
# create a class for views
###########
# dataName will be the modelName
setClass("genView", representation(dataName="character", win="GtkWindow",
         winNum="numeric"), contains=("VIRTUAL"))

setClass("plotView", representation(plotDevice="numeric", plotPar="list",
         drArea="GtkDrawingArea"), contains=c("genView", "VIRTUAL"))

# store the row names and column names rather than their indices
setClass("sPlotView", representation(dfRows="character", xvar="character",
         yvar="character"), contains="plotView")

setClass("qqPlotView", representation(xval="numeric", yval="numeric"),
          contains="plotView")

setClass("spreadView", representation(clist="GtkCList"), 
         contains="genView")

## 7/28/05 put the graphLayout info in the graphModel object
#setClass("graphView", representation(grLayout="Ragraph"), contains="plotView")
#
## 9/1/05 not sure if I need to store anything else about a heatmap
## decided to store the list of row and column reorderings returned from the
## heatmap function (just in case I need it later)
#setClass("heatmapView", representation(ordering="list"), contains="plotView")

#####
# accessor functions
#####
if (is.null(getGeneric("dataName")))
  setGeneric("dataName", function(object)
            standardGeneric("dataName"))
setMethod("dataName", "genView", function(object)
         object@dataName)

if (is.null(getGeneric("win")))
  setGeneric("win", function(object)
            standardGeneric("win"))
setMethod("win", "genView", function(object)
         object@win)

if (is.null(getGeneric("winNum")))
  setGeneric("winNum", function(object)
            standardGeneric("winNum"))
setMethod("winNum", "genView", function(object)
         object@winNum)

if (is.null(getGeneric("plotDevice")))
  setGeneric("plotDevice", function(object)
            standardGeneric("plotDevice"))
setMethod("plotDevice", "plotView", function(object)
         object@plotDevice)

if (is.null(getGeneric("plotPar")))
  setGeneric("plotPar", function(object)
            standardGeneric("plotPar"))
setMethod("plotPar", "plotView", function(object)
         object@plotPar)

if (is.null(getGeneric("drArea")))
  setGeneric("drArea", function(object)
            standardGeneric("drArea"))
setMethod("drArea", "plotView", function(object)
         object@drArea)

if (is.null(getGeneric("dfRows")))
  setGeneric("dfRows", function(object)
            standardGeneric("dfRows"))
setMethod("dfRows", "sPlotView", function(object)
         object@dfRows)

if (is.null(getGeneric("xvar")))
  setGeneric("xvar", function(object)
            standardGeneric("xvar"))
setMethod("xvar", "sPlotView", function(object)
         object@xvar)

if (is.null(getGeneric("yvar")))
  setGeneric("yvar", function(object)
            standardGeneric("yvar"))
setMethod("yvar", "sPlotView", function(object)
         object@yvar)

if (is.null(getGeneric("clist")))
  setGeneric("clist", function(object)
            standardGeneric("clist"))
setMethod("clist", "spreadView", function(object)
         object@clist)

if (is.null(getGeneric("xval")))
  setGeneric("xval", function(object)
            standardGeneric("xval"))
setMethod("xval", "qqPlotView", function(object)
         object@xval)

if (is.null(getGeneric("yval")))
  setGeneric("yval", function(object)
            standardGeneric("yval"))
setMethod("yval", "qqPlotView", function(object)
         object@yval)

#if (is.null(getGeneric("ordering")))
#  setGeneric("ordering", function(object)
#            standardGeneric("ordering"))
#setMethod("ordering", "heatmapView", function(object)
#         object@ordering)
#
#if (is.null(getGeneric("grLayout")))
#  setGeneric("grLayout", function(object)
#            standardGeneric("grLayout"))
#setMethod("grLayout", "graphView", function(object)
#         object@grLayout)

#####
# setting the slots
#####
if (is.null(getGeneric("dataName<-")))
  setGeneric("dataName<-",function(object, value)
            standardGeneric("dataName<-"))
setReplaceMethod("dataName","genView",function(object, value)
         {
           object@dataName<-value
           object
         }
)

if (is.null(getGeneric("win<-")))
  setGeneric("win<-",function(object, value)
            standardGeneric("win<-"))
setReplaceMethod("win","genView",function(object, value)
         {
           object@win<-value
           object
         }
)

if (is.null(getGeneric("winNum<-")))
  setGeneric("winNum<-",function(object, value)
            standardGeneric("winNum<-"))
setReplaceMethod("winNum","genView",function(object, value)
         {
           object@winNum<-value
           object
         }
)

if (is.null(getGeneric("plotDevice<-")))
  setGeneric("plotDevice<-",function(object, value)
            standardGeneric("plotDevice<-"))
setReplaceMethod("plotDevice","plotView",function(object, value)
         {
           object@plotDevice<-value
           object
         }
)

if (is.null(getGeneric("plotPar<-")))
  setGeneric("plotPar<-",function(object, value)
            standardGeneric("plotPar<-"))
setReplaceMethod("plotPar","plotView",function(object, value)
         {
           object@plotPar<-value
           object
         }
)

if (is.null(getGeneric("drArea<-")))
  setGeneric("drArea<-",function(object, value)
            standardGeneric("drArea<-"))
setReplaceMethod("drArea","plotView",function(object, value)
         {
           object@drArea<-value
           object
         }
)

if (is.null(getGeneric("dfRows<-")))
  setGeneric("dfRows<-",function(object, value)
            standardGeneric("dfRows<-"))
setReplaceMethod("dfRows","sPlotView",function(object, value)
         {
           object@dfRows<-value
           object
         }
)

if (is.null(getGeneric("xvar<-")))
  setGeneric("xvar<-",function(object, value)
            standardGeneric("xvar<-"))
setReplaceMethod("xvar","sPlotView",function(object, value)
         {
           object@xvar<-value
           object
         }
)

if (is.null(getGeneric("yvar<-")))
  setGeneric("yvar<-",function(object, value)
            standardGeneric("yvar<-"))
setReplaceMethod("yvar","sPlotView",function(object, value)
         {
           object@yvar<-value
           object
         }
)

if (is.null(getGeneric("clist<-")))
  setGeneric("clist<-",function(object, value)
            standardGeneric("clist<-"))
setReplaceMethod("clist","spreadView",function(object, value)
         {
           object@clist<-value
           object
         }
)

if (is.null(getGeneric("xval<-")))
  setGeneric("xval<-",function(object, value)
            standardGeneric("xval<-"))
setReplaceMethod("xval", "qqPlotView", function(object, value)
         {
           object@xval<-value
           object
         }
)

if (is.null(getGeneric("yval<-")))
  setGeneric("yval<-",function(object, value)
            standardGeneric("yval<-"))
setReplaceMethod("yval","qqPlotView", function(object, value)
         {
           object@yval<-value
           object
         }
)

#if (is.null(getGeneric("ordering<-")))
#  setGeneric("ordering<-", function(object, value)
#            standardGeneric("ordering<-"))
#setReplaceMethod("ordering", "heatmapView", function(object, value)
#         {
#           object@ordering<-value
#           object
#         }
#)
#
#if (is.null(getGeneric("grLayout<-")))
#  setGeneric("grLayout<-", function(object, value)
#            standardGeneric("grLayout<-"))
#setReplaceMethod("grLayout", "graphView", function(object, value)
#         {
#           object@grLayout<-value
#           object
#         }
#)

#####
# generic functions for gtk events on view objects
#####
if (is.null(getGeneric("motionEvent")))
  setGeneric("motionEvent", function(object, event,...)
            standardGeneric("motionEvent"))

if (is.null(getGeneric("clickEvent")))
  setGeneric("clickEvent", function(object, where, ...)
            standardGeneric("clickEvent"))

#########
# added 4/23/06
# to identify an object on a view (starting from user coordinates)
#########
if (is.null(getGeneric("identifyView")))
  setGeneric("identifyView", function(object, location, ...)
            standardGeneric("identifyView"))

#########
# added 6/5/05
# make method to update a view depending on the view object
# vData is the view data needed to update the view
#########
if (is.null(getGeneric("updateView")))
  setGeneric("updateView", function(object, vData)
            standardGeneric("updateView"))

########
# added 6/5/05
# make a method to redraw a view depending on the view object
########
if (is.null(getGeneric("redrawView")))
  setGeneric("redrawView", function(object)
            standardGeneric("redrawView"))






#############
# create a class for MVC objects
#############

setClass("MVC", representation(model="gModel", viewList="list", 
                  controller="environment"), contains=("VIRTUAL"))

setClass("singleModelMVC", contains="MVC")

setClass("linkedModelMVC", representation(parentMVC="character",
                           childMVCList="list"), contains="singleModelMVC")

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
setMethod("parentMVC", "linkedModelMVC", function(object)
         object@parentMVC)

if (is.null(getGeneric("childMVCList")))
  setGeneric("childMVCList", function(object)
            standardGeneric("childMVCList"))
setMethod("childMVCList", "linkedModelMVC", function(object)
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
setReplaceMethod("parentMVC", "linkedModelMVC", function(object,value)
         {
           object@parentMVC<-value
           object
         }
)

if (is.null(getGeneric("childMVCList<-")))
  setGeneric("childMVCList<-", function(object,value)
            standardGeneric("childMVCList<-"))
setReplaceMethod("childMVCList", "linkedModelMVC", function(object,value)
         {
           object@childMVCList<-value
           object
         }
)


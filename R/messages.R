###########
# create a class for messages
###########
# a virtual class for message
setClass("gMessage")

setClass("gUpdateMessage", representation(type="character", mData="list"),
         contains="gMessage")
setClass("gUpdateViewMessage", contains="gUpdateMessage")
setClass("gUpdateDataMessage", contains="gUpdateMessage")

setClass("gAddMessage", representation(dataName="character", mData="list",
         type="character"), contains="gMessage")
setClass("gAddViewMessage", contains="gAddMessage")
setClass("gAddDataMessage", contains="gAddMessage")

#####
# accessor functions
#####
if (is.null(getGeneric("type")))
  setGeneric("type", function(object)
            standardGeneric("type"))
setMethod("type", "gUpdateMessage", function(object)
         object@type)
setMethod("type", "gAddMessage", function(object)
         object@type)

if (is.null(getGeneric("mData")))
  setGeneric("mData", function(object)
            standardGeneric("mData"))
setMethod("mData", "gUpdateMessage", function(object)
         object@mData)
setMethod("mData", "gAddMessage", function(object)
         object@mData)

if (is.null(getGeneric("dataName")))
  setGeneric("dataName", function(object)
            standardGeneric("dataName"))
setMethod("dataName", "gAddMessage", function(object)
         object@dataName)

#####
# setting the slots
#####
if (is.null(getGeneric("type<-")))
  setGeneric("type<-", function(object, value)
            standardGeneric("type<-"))
setReplaceMethod("type", "gUpdateMessage", function(object, value)
         {
           object@type<-value
           object
         }
)
setReplaceMethod("type", "gAddMessage", function(object, value)
         {
           object@type<-value
           object
         }
)

if (is.null(getGeneric("mData<-")))
  setGeneric("mData<-",function(object, value)
            standardGeneric("mData<-"))
setReplaceMethod("mData", "gUpdateMessage", function(object, value)
         {
           object@mData<-value
           object
         }
)
setReplaceMethod("mData", "gAddMessage", function(object, value)
         {
           object@mData<-value
           object
         }
)

if (is.null(getGeneric("dataName<-")))
  setGeneric("dataName<-", function(object, value)
            standardGeneric("dataName<-"))
setReplaceMethod("dataName", "gAddMessage", function(object, value)
         {
           object@dataName<-value
           object
         }
)

#####
# initialize methods
#####
setMethod("initialize", "gUpdateViewMessage", 
  function(.Object, type, mData)
  {
    .Object@type<-type
    .Object@mData<-mData

    .Object
  }
)

#####
# have the 
# any type of view can update a dataframe - should this be 
# even more general???
#####
setMethod("initialize", "gUpdateDataMessage", 
  function(.Object, type, mData)
  {
    .Object@type<-type
    .Object@mData<-mData
    
    .Object
  }
)

#######
# data must be a list!!!
# data may contain only the model data or it may contain the model data AND
# the link data
#######
setMethod("initialize", "gAddDataMessage",
  function(.Object, data, dataName, type)
  {
    .Object@dataName<-dataName
    .Object@mData<-data
    .Object@type<-type
    
    .Object
  }
)

setMethod("initialize", "gAddViewMessage",
  function(.Object, dataName, type, ...)
  {
    .Object@mData<-list(...)
    .Object@dataName<-dataName
    .Object@type<-type

    .Object
  }
)

#####
# generic functions for handling messages
#####
if (is.null(getGeneric("handleMessage")))
  setGeneric("handleMessage", function(object, ...)
            standardGeneric("handleMessage"))

#####
#
#####
setMethod("handleMessage", "gUpdateViewMessage",
  function(object, ...)
  {
    type<-type(object)
    viewdata<-mData(object)

    # now need to update the model
    activeMVC<-get("activeMVC", mvcEnv)
    curMVC<-getMVC(activeMVC)
    curVList<-viewList(curMVC)
    controlEnv<-controller(curMVC)

    if (length(dev.list()) > 0)
      activeDev<-dev.cur()

    assign("contLoop", TRUE, controlEnv)
    controller(curMVC)<-controlEnv
    MVCList<-get("MVCList", mvcEnv)
    allNames <- getModelNames(sort = FALSE)
    index <- match(activeMVC, allNames)
    MVCList[[index]]<-curMVC
    assign("MVCList", MVCList, mvcEnv)

    if (type=="updateView")
    {
      for (i in 1:length(curVList))
      {
        updateView(curVList[[i]], viewdata) 
      }
    }
    if (type=="redrawView")
    {
      for (i in 1:length(curVList))
      {
        # for type of "redrawView", mData will be an empty list
        redrawView(curVList[i])
      }
    }

    activeMVC<-get("activeMVC", mvcEnv)
    curMVC<-getMVC(activeMVC)
    curVList<-viewList(curMVC)
    controlEnv<-controller(curMVC)

    assign("contLoop", FALSE, controlEnv)
    controller(curMVC)<-controlEnv
    MVCList<-get("MVCList", mvcEnv)
    allNames <- getModelNames(sort = FALSE)
    index <- match(activeMVC, allNames)
    MVCList[[index]]<-curMVC
    assign("MVCList", MVCList, mvcEnv)

    # reset the active device
    if (length(dev.list()) > 0)
      dev.set(activeDev)
  }
)

setMethod("handleMessage", "gUpdateDataMessage",
  function(object,...)
  {
    data<-mData(object)
    type<-type(object)

    # now need to update the model
    activeMVC<-get("activeMVC", mvcEnv)
    curMVC<-getMVC(activeMVC)
    curModel<-model(curMVC)

    # a method for each model to update its data
    viewdata<-updateModel(curModel, type, data)
 
    # update the views now that the data has changed
    # check that the model has views also!
    curVList<-viewList(curMVC)
    if (length(curVList) > 0)
    {
      # create an update view message
      # for now assume that all update view messages that come from update
      # data will have type "updateView", rather than "redrawView"
      uvMessage<-new("gUpdateViewMessage", type="updateView", mData=viewdata)
      handleMessage(uvMessage)
    }
  }
)

########
# 6/1/05
# load models through messages
########
setMethod("handleMessage", "gAddDataMessage",
  function(object, ...)
  {
#    print(object)
    data<-mData(object)$data
    linkData<-mData(object)$linkData
    name<-dataName(object)
    type<-type(object)

    loadModel(data, type, name, linkData)
  }
)
 
#######
# 5/24/05 want to initialize a view through methods rather than a function
#######
setMethod("handleMessage", "gAddViewMessage",
  function(object, ...)
  {
    dataName<-dataName(object)
    type<-type(object)
    mData<-mData(object)
#    print(mData)

    if (length(mData) > 0)
      newView<-new(type, dataName, mData)
    else
      newView<-new(type, dataName)
  }
)



###########
# create a class for messages
###########
# a virtual class for message
setClass("gMessage")

setClass("gUpdateMessage", representation(type="character", mData="list"),
         contains="gMessage")
setClass("gUpdateViewMessage", contains="gUpdateMessage")
setClass("gUpdateDataMessage", representation(to="character"), 
         contains="gUpdateMessage")

setClass("gAddMessage", representation(dataName="character", mData="list"), 
         contains="gMessage")
setClass("gAddViewMessage", representation(type="character"), 
         contains="gAddMessage")
setClass("gAddDataMessage", contains="gAddMessage")

#####
# accessor functions
#####
if (is.null(getGeneric("type")))
  setGeneric("type", function(object)
            standardGeneric("type"))
setMethod("type", "gUpdateMessage", function(object)
         object@type)
setMethod("type", "gAddViewMessage", function(object)
         object@type)

if (is.null(getGeneric("mData")))
  setGeneric("mData", function(object)
            standardGeneric("mData"))
setMethod("mData", "gUpdateMessage", function(object)
         object@mData)
setMethod("mData", "gAddMessage", function(object)
         object@mData)

if (is.null(getGeneric("to")))
  setGeneric("to", function(object)
            standardGeneric("to"))
setMethod("to", "gUpdateDataMessage", function(object)
         object@to)

if (is.null(getGeneric("dataName")))
  setGeneric("dataName", function(object)
            standardGeneric("dataName"))
setMethod("dataName", "gAddMessage", function(object)
         object@dataName)

#####
# setting the slots
#####
if (is.null(getGeneric("type<-")))
  setGeneric("type<-",function(object,value)
            standardGeneric("type<-"))
setReplaceMethod("type","gUpdateMessage",function(object,value)
         {
           object@type<-value
           object
         }
)
setReplaceMethod("type","gAddViewMessage", function(object,value)
         {
           object@type<-value
           object
         }
)

if (is.null(getGeneric("mData<-")))
  setGeneric("mData<-",function(object,value)
            standardGeneric("mData<-"))
setReplaceMethod("mData","gUpdateMessage",function(object,value)
         {
           object@mData<-value
           object
         }
)
setReplaceMethod("mData", "gAddMessage", function(object,value)
         {
           object@mData<-value
           object
         }
)

if (is.null(getGeneric("to<-")))
  setGeneric("to<-",function(object,value)
            standardGeneric("to<-"))
setReplaceMethod("to","gUpdateDataMessage",function(object,value)
         {
           object@to<-value
           object
         }
)

if (is.null(getGeneric("dataName<-")))
  setGeneric("dataName<-",function(object,value)
            standardGeneric("dataName<-"))
setReplaceMethod("dataName","gAddMessage",function(object,value)
         {
           object@dataName<-value
           object
         }
)

#####
# initialize methods
#####
setMethod("initialize", "gUpdateViewMessage", 
  function(.Object, type, ...)
  {
    # type is the type of change that was performed on the data (currently,
    # the data can only be dataframes)
    if (type=="reset")
    {
      .Object@type <- "replot"
      .Object@mData <- list()
    }
    if (type=="modify")
    {
      Rname<-list(...)$Rname
      .Object@type <- "updatePoints"
      .Object@mData <- list(Rname=Rname)
    }
    .Object
  }
)

#####
# have the 
# any type of view can update a dataframe - should this be 
# even more general???
#####
setMethod("initialize", "gUpdateDataMessage", 
  function(.Object, from, where, ...)
  {
    # put the update dataframe in a separate function in case the user
    # ever wants to update a different type of data, then it will be easy
    # to add a new function in place of updateDF
    if (is(from,"genView"))
      retList<-viewUpdateData(from, where, ...)

    .Object@type<-retList$type
    .Object@mData<-retList$mData
    .Object@to<-retList$to
    
    .Object
  }
)

setMethod("initialize", "gAddDataMessage",
  function(.Object, data, ...)
  {
    extraParam<-list(...)
    if (length(extraParam) > 0)
      dataName<-extraParam$dataName
    else
    {
      dataName<-data
      data<-eval(as.name(data))
    }      

    .Object@dataName<-dataName
    .Object@mData<-list(data=data)
    
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
  setGeneric("handleMessage", function(object,...)
            standardGeneric("handleMessage"))

setMethod("handleMessage", "gUpdateViewMessage",
  function(object,...)
  {
    # expect the data name to be in the ... parameter
    curDataName<-list(...)$dataName 

    # first determine which plots need to be updated
    viewList<-get("viewList",viewEnv)
    allDataNames<-unlist(lapply(viewList, function(x) {dataName(x)}))
    booPlots<-unlist(lapply(viewList, function(x) {is(x,"plotView")}))
    booSpread<-unlist(lapply(viewList, function(x) {is(x,"spreadView")}))

    # the indices in the viewList of the scatterplots and spreadsheet that
    # depend on the data name
    plotIndex<-which(booPlots & (allDataNames %in% curDataName))
    spreadIndex<-which(booSpread & (allDataNames %in% curDataName))

    type<-type(object)
    data<-mData(object)

    updatePlots(type,plotIndex,data) 

    updateSpread(curDataName,spreadIndex,data)    
  }
)

setMethod("handleMessage", "gUpdateDataMessage",
  function(object,...)
  {
    data<-mData(object)
    name<-to(object)
    type<-type(object)

    # could add a different function to change a different type of data

    # this function changes a dataframe  
    do.call("modify",data)
 
    # update the views now that the data has changed
    dfName<-data$dfName
    Rname<-data$Rname
    if (length(get("viewList",viewEnv)) > 0)
      updateViews(dfName,type,Rname)
  }
)

setMethod("handleMessage", "gAddDataMessage",
  function(object,...)
  {
    curList<-mData(object)
    data<-curList$data
    name<-dataName(object)

    loadDFData(data,name)
    checkIfPlotted(name)
  }
)
 
setMethod("handleMessage", "gAddViewMessage",
  function(object,...)
  {
    dataName<-dataName(object)
    type<-type(object)
    mData<-mData(object)

    mData$dataName<-dataName
    mData$type<-type
   
    do.call("createView",mData)
#    createView(type=type, dataName=dataName, mData)    
  }
)


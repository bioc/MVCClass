###########
# create a class for views
###########
# dataName will be the modelName
setClass("genView", representation(dataName="character", win="GtkWindow",
         winNum="numeric"), contains=("VIRTUAL"))

setClass("plotView", representation(plotDevice="numeric", plotPar="list",
         drArea="GtkDrawingArea"), contains="genView")
#setClass("sPlotView", representation(dfRows="numeric", colx="numeric", 
#         coly="numeric"), contains="plotView")
# store the row names and column names rather than their indices
setClass("sPlotView", representation(dfRows="character", colx="character",
         coly="character"), contains="plotView")

setClass("spreadView", representation(clist="GtkCList"), 
         contains="genView")

setClass("graphView", representation(graphLayout="Ragraph"),
                          contains="plotView")

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

if (is.null(getGeneric("colx")))
  setGeneric("colx", function(object)
            standardGeneric("colx"))
setMethod("colx", "sPlotView", function(object)
         object@colx)

if (is.null(getGeneric("coly")))
  setGeneric("coly", function(object)
            standardGeneric("coly"))
setMethod("coly", "sPlotView", function(object)
         object@coly)

if (is.null(getGeneric("clist")))
  setGeneric("clist", function(object)
            standardGeneric("clist"))
setMethod("clist", "spreadView", function(object)
         object@clist)

if (is.null(getGeneric("graphLayout")))
  setGeneric("graphLayout", function(object)
            standardGeneric("graphLayout"))
setMethod("graphLayout", "graphView", function(object)
         object@graphLayout)

#####
# setting the slots
#####
if (is.null(getGeneric("dataName<-")))
  setGeneric("dataName<-",function(object,value)
            standardGeneric("dataName<-"))
setReplaceMethod("dataName","genView",function(object,value)
         {
           object@dataName<-value
           object
         }
)

if (is.null(getGeneric("win<-")))
  setGeneric("win<-",function(object,value)
            standardGeneric("win<-"))
setReplaceMethod("win","genView",function(object,value)
         {
           object@win<-value
           object
         }
)

if (is.null(getGeneric("winNum<-")))
  setGeneric("winNum<-",function(object,value)
            standardGeneric("winNum<-"))
setReplaceMethod("winNum","genView",function(object,value)
         {
           object@winNum<-value
           object
         }
)

if (is.null(getGeneric("plotDevice<-")))
  setGeneric("plotDevice<-",function(object,value)
            standardGeneric("plotDevice<-"))
setReplaceMethod("plotDevice","plotView",function(object,value)
         {
           object@plotDevice<-value
           object
         }
)

if (is.null(getGeneric("plotPar<-")))
  setGeneric("plotPar<-",function(object,value)
            standardGeneric("plotPar<-"))
setReplaceMethod("plotPar","plotView",function(object,value)
         {
           object@plotPar<-value
           object
         }
)

if (is.null(getGeneric("drArea<-")))
  setGeneric("drArea<-",function(object,value)
            standardGeneric("drArea<-"))
setReplaceMethod("drArea","plotView",function(object,value)
         {
           object@drArea<-value
           object
         }
)

if (is.null(getGeneric("dfRows<-")))
  setGeneric("dfRows<-",function(object,value)
            standardGeneric("dfRows<-"))
setReplaceMethod("dfRows","sPlotView",function(object,value)
         {
           object@dfRows<-value
           object
         }
)

if (is.null(getGeneric("colx<-")))
  setGeneric("colx<-",function(object,value)
            standardGeneric("colx<-"))
setReplaceMethod("colx","sPlotView",function(object,value)
         {
           object@colx<-value
           object
         }
)

if (is.null(getGeneric("coly<-")))
  setGeneric("coly<-",function(object,value)
            standardGeneric("coly<-"))
setReplaceMethod("coly","sPlotView",function(object,value)
         {
           object@coly<-value
           object
         }
)

if (is.null(getGeneric("clist<-")))
  setGeneric("clist<-",function(object,value)
            standardGeneric("clist<-"))
setReplaceMethod("clist","spreadView",function(object,value)
         {
           object@clist<-value
           object
         }
)

if (is.null(getGeneric("graphLayout<-")))
  setGeneric("graphLayout<-", function(object,value)
            standardGeneric("graphLayout<-"))
setReplaceMethod("graphLayout", "graphView", function(object,value)
         {
           object@graphLayout<-value
           object
         }
)

#####
# generic functions for gtk events on view objects
#####
if (is.null(getGeneric("motionEvent")))
  setGeneric("motionEvent", function(object, event,...)
            standardGeneric("motionEvent"))

if (is.null(getGeneric("clickEvent")))
  setGeneric("clickEvent", function(object, where, ...)
            standardGeneric("clickEvent"))

if (is.null(getGeneric("viewUpdateData")))
  setGeneric("viewUpdateData", function(object, where, ...)
            standardGeneric("viewUpdateData"))

# added 3/28/05
if (is.null(getGeneric("identifyLoc")))
  setGeneric("identifyLoc", function(object, xyloc)
            standardGeneric("identifyLoc"))

setMethod("identifyLoc", "sPlotView",
  function(object, xyloc)
  {
    identifyPoint(object, xyloc)
  }
)

##########
# update the data based on interacting with a spreadsheet view
##########
#setMethod("viewUpdateData", "spreadView",
#  function(object, where, ...)
#  {
#    # need to get the class of the model object
#    # updateDFBySpread is expecting a data frame model
#    if (is(getModel(dataName(object)), "dfModel"))
#      retList<-updateDFBySpread(object, where, ...)
#
#    return(retList)
#  }
#)

setMethod("viewUpdateData", "sPlotView",
  function(object, where, ...)
  {
    # need to get the class of the model object
    # updateDFBysPlot is expecting a data frame model
    if (is(getModel(dataName(object)), "dfModel"))
      retList<-updateDFBysPlot(object, where)

    return(retList)
  }
)

########
# clickEvent for spreadView is called in response to a selected row
# or unselected row from the clist on a spreadView
########
setMethod("clickEvent", "spreadView", 
  function(object, where, ...)
  {
    cont<-TRUE
    extras<-list(...)
    event<-extras$event
    viewMode<-get("viewMode",controlEnv)
    if (viewMode == "")
      cont<-FALSE
    # we don't need to update the data if the view mode is color and a
    # row has been unselected
    if (viewMode=="color" && event=="unselect")
      cont<-FALSE

    # only update the data if view mode is set
    if (cont)
    {
      # where will be the list item that was clicked 
      dfMessage<-new("gUpdateDataMessage", from=object, where=where, ...)
      handleMessage(dfMessage)
    }
  }
)

#########
# create a button press event for scatterplot views (used to be
# addButtonPress function)
#########
setMethod("clickEvent", "sPlotView",
  function(object, where, ...)
  {
    activeMVC<-get("activeMVC", mvcEnv)
    curMVC<-getMVC(activeMVC)
    controlEnv<-controller(curMVC)

    # see if there is a function to call
    rightButtonClick<-get("rightButtonClick", controlEnv)
    leftButtonClick<-get("leftButtonClick", controlEnv)
    middleButtonClick<-get("middleButtonClick", controlEnv)

    # see if any of the functions are active
    activeRBCFun<-unlist(lapply(rightButtonClick, function(y)
      {y$assigned==TRUE}))
    activeLBCFun<-unlist(lapply(leftButtonClick, function(y)
      {y$assigned==TRUE}))
    activeMBCFun<-unlist(lapply(middleButtonClick, function(y)
      {y$assigned==TRUE}))

    # where will be the mouse click event information from Gtk
    # check that the left button was pressed
    if (gdkEventButtonGetButton(where)==1)
    {
      if (any(activeLBCFun))
      {
        usri <- pix2inches(where[["X"]], where[["Y"]])
        usrc <- inches2usr(usri$x, usri$y)

        curPoint<-identifyPoint(object, usrc)

        if (!is.null(curPoint))
        {
#          # if a point was clicked, then we want to create a 
#          # gUpdateDataMessage
#          dfMessage<-new("gUpdateDataMessage", from=object, where=curPoint)
#          handleMessage(dfMessage)

          curIndex<-which(activeLBCFun)
          if (length(curIndex)==1)
          { 
            curEventFun<-mouseOver[[curIndex]][["eventFun"]]

            # now call the function the user has set with the current
            # node as the parameter - assume the function does not return
            # anything to the motion notify event
            actualFun<-callFun(curEventFun)
            do.call(actualFun, list(curPoint$closestPoint))
          }

        }
      }
    }

    if (gdkEventButtonGetButton(where)==2)
    {
      if (any(activeMBCFun))
      {
        usri <- pix2inches(where[["X"]], where[["Y"]])
        usrc <- inches2usr(usri$x, usri$y)

        curPoint<-identifyPoint(object, usrc)

        if (!is.null(curPoint))
        {
          # if a point was clicked, then we want to create a 
          # gUpdateDataMessage
          dfMessage<-new("gUpdateDataMessage", from=object, where=curPoint)
          handleMessage(dfMessage)
        }
      }
    }

    if (gdkEventButtonGetButton(where)==3)
    {
      if (any(activeRBCFun))
      {
        usri <- pix2inches(where[["X"]], where[["Y"]])
        usrc <- inches2usr(usri$x, usri$y)

        curPoint<-identifyPoint(object, usrc)

        if (!is.null(curPoint))
        {
          # if a point was clicked, then we want to create a 
          # gUpdateDataMessage
          dfMessage<-new("gUpdateDataMessage", from=object, where=curPoint)
          handleMessage(dfMessage)
        }
      }
    }
  }
)

########
# a method for a motion notify event on a scatterplot
########
setMethod("motionEvent", "sPlotView",
  function(object, event, ...)
  {
    activeMVC<-get("activeMVC", mvcEnv)
    curMVC<-getMVC(activeMVC)
    controlEnv<-controller(curMVC)

    # see if there is a function to call
    mouseOver<-get("mouseOver", controlEnv)
    # see if any of the functions are active
    activeFun<-unlist(lapply(mouseOver, function(y){y$assigned==TRUE}))

    # then there is an active function
    if (any(activeFun))
 #   # only respond to motion events for identify view mode
 #   if (get("viewMode", controlEnv)=="identify")
    {
      curWin<-win(object)
      # potential problem - these depend on how the 
      # scatterplot window is set up
      # SHOULD AT LEAST CHECK THEIR CLASSES!!
      hrul<-gtkContainerGetChildren(gtkContainerGetChildren(curWin)[[1]])[[1]]
      vrul<-gtkContainerGetChildren(gtkContainerGetChildren(
             gtkContainerGetChildren(curWin)[[1]])[[2]])[[1]]

      hrul$SignalEmit("motion-notify-event", event)
      vrul$SignalEmit("motion-notify-event", event)
      
      curx<-gdkEventMotionGetX(event)
      cury<-gdkEventMotionGetY(event)

      xyloc<-list(x=curx, y=cury)

      # convert to user coordinates
      xyInches<-pix2inches(curx, cury)
      xyUsr<-inches2usr(xyInches$x, xyInches$y)
 
      # also need to ensure that the plot has been added to viewList
      curDev<-dev.cur()
      curViewList<-viewList(curMVC)
      plotDev<-unlist(lapply(curViewList, function(x) {plotDevice(x)}))     
#      plotDev<-getPlotDev(getPlotsFromViews())
      if (curDev %in% plotDev)
      {
#        checkPoint(curx, cury, object)
        curPoint<-identifyPoint(object, xyUsr)

        curIndex<-which(activeFun)
        if (length(curIndex)==1)
        {
          curEventFun<-mouseOver[[curIndex]][["eventFun"]]

          # now call the function the user has set with the current
          # node as the parameter - assume the function does not return
          # anything to the motion notify event
          actualFun<-callFun(curEventFun)
          do.call(actualFun, list(curPoint$closestPoint))
        }
      }
    }
  }
)

##########
# 5/24/05
# need to create an initialize method that will be called
# when creating a view
##########

# dataName, dfRows, colx and coly are the only data needed to create scatterplot
setMethod("initialize", "sPlotView", 
  function(.Object, dataName, mData)
  {
    dfRows<-mData$dfRows
    colx<-mData$colx
    coly<-mData$coly

    # create slot values
    retList<-initSPlotView()

    # fill the slots in the object
    .Object@dataName<-dataName
    .Object@win<-retList$win
    .Object@plotDevice<-retList$plotDevice
    .Object@plotPar<-retList$plotPar
    .Object@drArea<-retList$drArea
    .Object@dfRows<-dfRows
    .Object@colx<-colx
    .Object@coly<-coly

    # create the actual plot
    .Object<-scatterplot(.Object)

    # finally add events to the plot
    .Object<-addEventsforViews(.Object)
    addEventsforSPlots(.Object)

    .Object
  }
)

setMethod("initialize", "spreadView", 
  function(.Object, dataName)
  {
    # create the slot values
    retList<-initSpreadView(dataName)
   
    # fill the slots in the object
    .Object@dataName<-dataName
    .Object@win<-retList$win
    .Object@clist<-retList$clist

    # finally add events to the view
    .Object<-addEventsforViews(.Object)
    addEventsforSpread(.Object)

    .Object
  }
)

setMethod("initialize", "graphView",
  function(.Object, dataName, mData)
  {
    glayout<-mData$glayout
    nShape<-mData$nShape

    # create slot values
    retList<-initGraphView(dataName, glayout, nShape)

    # fill the slots in the object
    .Object@dataName<-dataName
    .Object@win<-retList$win
    .Object@plotDevice<-retList$plotDevice
    .Object@plotPar<-retList$plotPar
    .Object@drArea<-retList$drArea
    .Object@graphLayout<-retList$graphLayout

    # create the actual plot
    .Object<-graphPlot(.Object)

    # finally add events to the plot
    .Object<-addEventsforViews(.Object)
    addEventsforGraphs(.Object)

    .Object    
  }
)

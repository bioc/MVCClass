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
    # only update the data if view mode is set and if the view mode is not
    # identify - in identify mode only pay attention to motion event
    viewMode<-get("viewMode", controlEnv)
    if (viewMode != "" && viewMode != "identify")
    {
      #where will be the mouse click event information from Gtk
    
      # check that the left button was pressed
      if (gdkEventButtonGetButton(where)==1)
      {
        usri <- pix2inches(where[["X"]], where[["Y"]])
        usrc <- inches2usr(usri$x, usri$y)

#        viewMode<-get("viewMode",controlEnv)

        curPoint<-identifyPoint(object,usrc)

        if (!is.null(curPoint))
        {
          # if a point was clicked, then we want to create a 
          # gUpdateDataMessage
          dfMessage<-new("gUpdateDataMessage", from=object, where=curPoint)
          handleMessage(dfMessage)
        }
      }
      # may want to add functionality if the right or middle 
      # buttons are pressed

      # for right click have a widget pop up that asks the 
      # user which interact function to use??
      if (gdkEventButtonGetButton(where)==3)
      {
        
      }
    }
  }
)

########
# a method for a motion notify event on a scatterplot
########
setMethod("motionEvent", "sPlotView",
  function(object,event,...)
  {
    # only respond to motion events for identify view mode
    if (get("viewMode",controlEnv)=="identify")
    {
      curWin<-win(object)
      # potential problem - these depend on how the 
      # scatterplot window is set up
      # SHOULD AT LEAST CHECK THEIR CLASSES!!
      hrul<-gtkContainerGetChildren(gtkContainerGetChildren(curWin)[[1]])[[1]]
      vrul<-gtkContainerGetChildren(gtkContainerGetChildren(
             gtkContainerGetChildren(curWin)[[1]])[[2]])[[1]]

      hrul$SignalEmit("motion-notify-event",event)
      vrul$SignalEmit("motion-notify-event",event)
      
      curx<-gdkEventMotionGetX(event)
      cury<-gdkEventMotionGetY(event)

      # also need to ensure that the plot has been added to viewList
      curDev<-dev.cur()
      plotDev<-getPlotDev(getPlotsFromViews())
      if (curDev %in% plotDev)
      {
        checkPoint(curx,cury,object)
      }
    }
  }
)
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

#if (is.null(getGeneric("viewUpdateData")))
#  setGeneric("viewUpdateData", function(object, where, ...)
#            standardGeneric("viewUpdateData"))

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

#########
# added 6/5/05
# make method to update a view depending on the view object
# vData is the view data needed to update the view
#########
if (is.null(getGeneric("updateView")))
  setGeneric("updateView", function(object, vData)
            standardGeneric("updateView"))

setMethod("updateView", "sPlotView",
  function(object, vData)
  {
    # vData is a list with 4 elements: rowName, colName, oldValue and newValue
    # will need to remove old value and draw point with new value

    # get the model info.
    dataName<-dataName(object)
    curMVC<-getMVC(dataName)
    model<-model(curMVC)
    modelData<-modelData(model)
    virtualData<-virtualData(model)

    # set the current device
    curDevice<-plotDevice(object)
    dev.set(curDevice)

    # will need the background color
    bgColor<-plotPar(object)$bg
    if (bgColor=="transparent")
      bgColor<-"white"

    rowName<-vData$rowName
    # this will be a column in the virtualData slot
    colName<-vData$colName
    oldValue<-vData$oldValue
    newValue<-vData$newValue

    # these are columns in the modelData slot
    colx<-colx(object)
    coly<-coly(object)
    dfRows<-dfRows(object)
    # only update this point if it is being shown in the current plot
    if (all(rowName %in% dfRows))
    {
      # get the x and y values
      rowIndex<-match(rowName, rownames(modelData))
      xcolIndex<-match(colx, colnames(modelData))
      ycolIndex<-match(coly, colnames(modelData))
      xvalue<-modelData[rowIndex, xcolIndex]
      yvalue<-modelData[rowIndex, ycolIndex]

      pchIndex<-match("pch", colnames(virtualData))
      pchValue<-virtualData[rowIndex, pchIndex]
      colIndex<-match("color", colnames(virtualData))
      colValue<-virtualData[rowIndex, colIndex]
      highIndex<-match("highlit", colnames(virtualData))
      highValue<-virtualData[rowIndex, highIndex]

      # next remove the old point
      # what about highlighting??? - would also need to remove this!
      points(xvalue, yvalue, col=bgColor, pch=pchValue)

      # remove highlighting if it's needed
      if (all(highValue==TRUE) && colName!="highlit")
        points(xvalue, yvalue, col=bgColor, cex=2, pch=1)
      if (colName=="highlit" && all(newValue==FALSE))
        points(xvalue, yvalue, col=bgColor, cex=2, pch=1)

      # only redraw the point if it's not supposed to be hidden
      if (!(colName=="hide" && all(newValue==TRUE))) 
      {
        # then redraw the point
        points(xvalue, yvalue, col=colValue, pch=pchValue)

        # add highlighting if it's needed
        if (all(highValue==TRUE))
          points(xvalue, yvalue, col="red", pch=1, cex=2)
      }
    }
  }
)

#########
#
#########
setMethod("updateView", "spreadView",
  function(object, vData)
  {
    # vData is a list with 4 elements: rowName, colName, oldValue and newValue

    # get the model info. - may not be from the active MVC
    curMVC<-getMVC(dataName(object))
    model<-model(curMVC)
    modelData<-modelData(model)
    virtualData<-virtualData(model)

    curCList<-clist(object)

    colName<-vData$colName
    rowName<-vData$rowName
    rowIndex<-match(rowName, rownames(modelData))
    
    if (colName=="color" || colName=="pch")
    {
      # always select the row when a point is being colored
      gtkCListSelectRow(curCList, rowIndex-1, -1)
    }
    else
    {
      # for highlight and hide need to toggle the selection
      # if value==TRUE then select, otherwise unselect
      newValue<-vData$newValue
      if (newValue==TRUE)
        gtkCListSelectRow(curCList, rowIndex-1, -1)
      else
      {
        gtkCListUnselectRow(curCList, rowIndex-1, -1)
      }
    }    
  }
)

setMethod("updateView", "graphView",
  function(object, vData)
  {}
)

########
# added 6/5/05
# make a method to redraw a view depending on the view object
########
if (is.null(getGeneric("redrawView")))
  setGeneric("redrawView", function(object)
            standardGeneric("redrawView"))

########
# clickEvent for spreadView is called in response to a selected row
# or unselected row from the clist on a spreadView
########
setMethod("clickEvent", "spreadView", 
  function(object, where, ...)
  {
    extras<-list(...)
    # event is select or unselect
    event<-extras$event

    # assume that the clickEvent for spreadView has the same callback
    # function as the left button click because a row can only be selected
    # or unselected with the left button

    # may want to check if the view clicked on is from the activeMVC
    # for now only allow views from the activeMVC to respond to click events
    activeMVC<-get("activeMVC", mvcEnv)
    if (dataName(object) == activeMVC)
    {
      curMVC<-getMVC(dataName(object))
      controlEnv<-controller(curMVC)
      modelData<-modelData(model(curMVC))
      rowName<-rownames(modelData)[where]

      # see if there is a function to call
      leftButtonClick<-get("leftButtonClick", controlEnv)

      # see if any of the functions are active
      activeLBCFun<-unlist(lapply(leftButtonClick, function(y)
        {y$assigned==TRUE}))

      curIndex<-which(activeLBCFun)
      if (length(curIndex)==1)
      { 
        curEventFun<-leftButtonClick[[curIndex]][["eventFun"]]

        actualFun<-callFun(curEventFun)
        # problem is that we can't list it as select or unselect event
        # but that's ok because highlight and hide will be toggled anyway
        do.call(actualFun, list(rowName))
      }
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
    # may want to check if the view clicked on is from the activeMVC
    # for now only allow views from the activeMVC to respond to click events
    activeMVC<-get("activeMVC", mvcEnv)
    if (dataName(object) == activeMVC)
    {
      curMVC<-getMVC(dataName(object))
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
#            # if a point was clicked, then we want to create a 
#            # gUpdateDataMessage
#            dfMessage<-new("gUpdateDataMessage", from=object, where=curPoint)
#            handleMessage(dfMessage)

            curIndex<-which(activeLBCFun)
            if (length(curIndex)==1)
            { 
              curEventFun<-leftButtonClick[[curIndex]][["eventFun"]]

              # now call the function the user has set with the current
              # point as the parameter
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
            curIndex<-which(activeMBCFun)
            if (length(curIndex)==1)
            { 
              curEventFun<-middleButtonClick[[curIndex]][["eventFun"]]

              # now call the function the user has set with the current
              # point as the parameter 
              actualFun<-callFun(curEventFun)
              do.call(actualFun, list(curPoint$closestPoint))
            }
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
            curIndex<-which(activeRBCFun)
            if (length(curIndex)==1)
            {
              curEventFun<-rightButtonClick[[curIndex]][["eventFun"]]
  
              # now call the function the user has set with the current
              # point as the parameter
              actualFun<-callFun(curEventFun)
              do.call(actualFun, list(curPoint$closestPoint))
            }
          }
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
    # may want to check if the view clicked on is from the activeMVC
    # for now only allow views from the activeMVC to respond to click events
    activeMVC<-get("activeMVC", mvcEnv)
    if (dataName(object) == activeMVC)
    {
      curMVC<-getMVC(dataName(object))
      controlEnv<-controller(curMVC)

      # see if there is a function to call
      mouseOver<-get("mouseOver", controlEnv)
      # see if any of the functions are active
      activeFun<-unlist(lapply(mouseOver, function(y){y$assigned==TRUE}))

      # then there is an active function
      if (any(activeFun))
      {
        curWin<-win(object)
        # potential problem - these depend on how the 
        # scatterplot window is set up
        # SHOULD AT LEAST CHECK THEIR CLASSES!!
        hrul<-gtkContainerGetChildren(
             gtkContainerGetChildren(curWin)[[1]])[[1]]
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

        devicesInView<-unlist(lapply(curViewList, function(x) {is(x,
                             "plotView")}))
        # only look at views that have devices
        viewsWithDevices<-curViewList[devicesInView]
        plotDev<-unlist(lapply(viewsWithDevices, function(x) {plotDevice(x)}))

        if (curDev %in% plotDev)
        {
#          checkPoint(curx, cury, object)
          curPoint<-identifyPoint(object, xyUsr)

          curIndex<-which(activeFun)
          if (length(curIndex)==1)
          {
            curEventFun<-mouseOver[[curIndex]][["eventFun"]]

            # now call the function the user has set with the current
            # point as the parameter - assume the function does not return
            # anything to the motion notify event
            actualFun<-callFun(curEventFun)
            do.call(actualFun, list(curPoint$closestPoint))
          }
        }
      }
    }
  }
)

#############
# added 6/28/05 so that the graphView has a motionEvent
#############
setMethod("motionEvent", "graphView",
  function(object, event, ...)
  {
    # may want to check if the view clicked on is from the activeMVC
    # for now only allow views from the activeMVC to respond to click events
    activeMVC<-get("activeMVC", mvcEnv)
    if (dataName(object) == activeMVC)
    {
      curMVC<-getMVC(dataName(object))
      controlEnv<-controller(curMVC)

      # see if there is a function to call
      mouseOver<-get("mouseOver", controlEnv)
      # see if any of the functions are active
      activeFun<-unlist(lapply(mouseOver, function(y){y$assigned==TRUE}))

      # then there is an active function
      if (any(activeFun))
      {
        curWin<-win(object)
        # potential problem - these depend on how the 
        # plot window is set up
        # SHOULD AT LEAST CHECK THEIR CLASSES!!
        hrul<-gtkContainerGetChildren(
             gtkContainerGetChildren(curWin)[[1]])[[1]]
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

        devicesInView<-unlist(lapply(curViewList, function(x) {is(x,
                             "plotView")}))
        # only look at views that have devices
        viewsWithDevices<-curViewList[devicesInView]
        plotDev<-unlist(lapply(viewsWithDevices, function(x) {plotDevice(x)}))

        if (curDev %in% plotDev)
        {
          curNode<-identifyNode(object, xyUsr)

          curIndex<-which(activeFun)
          if (length(curIndex)==1)
          { 
            curEventFun<-mouseOver[[curIndex]][["eventFun"]]

            # now call the function the user has set with the current
            # node as the parameter - assume the function does not return
            # anything to the motion notify event
            actualFun<-callFun(curEventFun)
            do.call(actualFun, list(curNode))
          }
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

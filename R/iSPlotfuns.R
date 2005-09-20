#########
# some functions from iSPlot
# and some are new
#########

##################
# identifyPoint determines which point on the scatterplot
# the user clicked (or NULL if no point was clicked)
#
# curplot is the plot information (one element from viewList)
# xyloc is the x and y location that was clicked
#
# this is called by clickEvent and motionEvent for sPlotView
##################
identifyPoint<-function(curplot, xyloc)
{
  # get the cxy of the active plot
  cxy<-par("cxy")
  pointWidth<-cxy[1]
  pointHeight<-cxy[2]  

  plotDf<-dataName(curplot)

  # these will be the row and column names
  plotRows<-dfRows(curplot)
  plotX<-colx(curplot)
  plotY<-coly(curplot)

  # need to get the data from the model
  mvcList<-get("MVCList", mvcEnv)
  
  curData<-getData(modelName=plotDf)
  rowIndices<-match(plotRows, rownames(curData))
  colxIndex<-match(plotX, colnames(curData))
  colyIndex<-match(plotY, colnames(curData))

  x<-curData[rowIndices, colxIndex]
  y<-curData[rowIndices, colyIndex]

  # check if x or y is a factor
  if (is(x,"factor"))
    x<-as.numeric(x)
  if (is(y,"factor"))
    y<-as.numeric(y)

  rowNames<-rownames(curData)

  # now need to identify the closest point
  totdiff<-(xyloc$x-x)^2+(xyloc$y-y)^2
  booClosest<-min(totdiff)==totdiff
  
  closestPoint<-rowNames[booClosest]
  closestXY<-list(x=x[booClosest], y=y[booClosest])
#  print(closestXY)

  # need to ensure that the closestPoint was actually clicked
  # use cxy
  x.within<-FALSE
  y.within<-FALSE

  if ((xyloc$x<=closestXY$x) && (xyloc$x>=closestXY$x-(pointWidth/2)))
      x.within<-TRUE
  if ((xyloc$x>closestXY$x) && (xyloc$x<=closestXY$x+(pointWidth/2)))
      x.within<-TRUE  

  if ((xyloc$y<=closestXY$y) && (xyloc$y>=closestXY$y-(pointHeight/2)))
      y.within<-TRUE
  if ((xyloc$y>closestXY$y) && (xyloc$y<=closestXY$y+(pointHeight/2)))
      y.within<-TRUE 

  if (x.within && y.within)
    return(list(closestXY=closestXY, closestPoint=closestPoint))
  else
    return(NULL)
}

######
# make an interface for getting data, in case new
# data types are added besides dataframe or matrix
#
# used by identifyPoint
######
getData<-function(modelName)
{
  curMVC<-getMVC(modelName)
  curModel<-model(curMVC)
  curData<-modelData(curModel)

  return(curData)
}

#########
# 5/24/05 create a graph view through initialize method
#
# called by initialize method for a graphView object
#########
initGraphView<-function(dataName, glayout, nShape)
{
  win<-gtkWindow(show=FALSE)

  # this will create a gtk device on the window
  retList<-createGtkDev(win)
  win<-retList$win
  drArea<-retList$drArea

  curMVC<-getMVC(dataName)
  virData<-virtualData(model(curMVC))

  win$Show()

  # set up the graph layout variable using agopen
  # temporarily set the shape and width so won't get font size errors
  nodeShape<-nShape

  # the graph data
  actGraph<-getData(modelName=dataName)
  Sys.sleep(0.5)

  # for now make the labels on the nodes blank (because having trouble
  # plotting them)
  if (is.null(virData))
  {
    curlayout<-agopen(actGraph, name=dataName, nodeAttrs=makeNodeAttrs
               (actGraph, label="", shape=nodeShape, fillcolor="transparent"))
  }
  else
  {
    # use the fillcolors that are already in the virtualData slot
    # not sure if other view info should be used (in addition to fillcolor,
    # like color)
    curFillColors<-unlist(lapply(AgNode(virData), fillcolor))
    curColors<-unlist(lapply(AgNode(virData), color))
#    print(curFillColors)
    curlayout<-agopen(actGraph, name=dataName, nodeAttrs=makeNodeAttrs
               (actGraph, label="", shape=nodeShape, color=curColors, 
                fillcolor=curFillColors))
  }

  newRetList<-list(win=win, drArea=drArea, plotDevice=dev.cur(),
                   plotPar=par(no.readonly=TRUE), graphLayout=curlayout)
  return(newRetList)
}

#########
# 5/24/05 create a scatterplot view through initialize method
# here just setting up the device
#
# called by initialize method for heatmapView and sPlotView
#########
initSPlotView<-function()
{
  win<-gtkWindow(show=FALSE)
  
  # this will create a gtk device on the window
  retList<-createGtkDev(win)
  win<-retList$win
  drArea<-retList$drArea

  newRetList<-list(win=win, drArea=drArea, plotDevice=dev.cur(),
                   plotPar=par(no.readonly=TRUE))

  return(newRetList)
}

#########
# 5/24/05 create a spreadsheet view through initialize method
#
# called by initialize method for spreadView
#########
initSpreadView<-function(dfName)
{
  win<-gtkWindow(show=FALSE)
  win$SetUsize(500, 400)
  newTable<-gtkTable(1, 1, TRUE)

  # actually get the data
  showdata<-getData(modelName=dfName)
  # need to add one to include the row names as a column
  numCols<-(dim(showdata)[2])+1
  numRows<-dim(showdata)[1]
  # need to add a frame that shows the data
  frData<-gtkFrame(dfName)     

  boxForShow<-gtkHBox(TRUE, 2)
  scShow<-gtkScrolledWindow()
  scShow$SetPolicy("automatic", "automatic")

  showDfList<-gtkCList(numCols, titles=c("Row.Names", names(showdata)))
  showDfList$Freeze()
  for (i in 1:numRows)
  {
    text<-c(row.names(showdata)[i], as.matrix(showdata[i,]))
    showDfList$Append(text)
  }
  showDfList$Thaw()
  # make the column titles just be labels rather than buttons
  gtkCListColumnTitlesPassive(showDfList)
  # also want to set the column width so all items in the column
  # can be seen
  gtkCListColumnsAutosize(showDfList)
  # set the selection mode to multiple so multiple items can be selected
  # at a time (not sure if I want extended)
  gtkCListSetSelectionMode(showDfList, GtkSelectionMode[3])

  scShow$AddWithViewport(showDfList)
  boxForShow$PackStart(scShow, expand=TRUE)
  frData$Add(boxForShow)
  gtkTableAttach(newTable, frData, 0, 1, 0, 1, ypadding=10, xpadding=5)
  win$Add(newTable)
  win$Show()

  newRetList<-list(win=win, clist=showDfList)

  return(newRetList)
}

#########
# 5/24/05 add events specifically for spreadsheets
#
# called by initialize method for spreadView
#########
addEventsforSpread<-function(curView)
{
  activeMVC<-get("activeMVC", mvcEnv)
  curMVC<-getMVC(activeMVC)
  controlEnv<-controller(curMVC)

  curclist<-clist(curView)
  gtkAddCallback(curclist, "select-row",
    function(obj, row, col, ev)
    {
      # need this variable to make sure don't end up in an infinite loop
      if (get("contLoop", controlEnv)==FALSE)
        clickEvent(curView, (row+1), event="select")
      return(TRUE)
    }
  )
   
  gtkAddCallback(curclist, "unselect-row",
    function(obj, row, col, ev)
    {
      # need this variable to make sure don't end up in an infinite loop
      if (get("contLoop", controlEnv)==FALSE)
        clickEvent(curView, (row+1), event="unselect")
      return(TRUE)
    }
  )
}

##########
# 5/24/05 add events specifically for scatterplots
#
# called by initialize method for sPlotView
##########
addEventsforSPlots<-function(curView)
{
  gtkAddCallback(drArea(curView), "button_press_event",
    function(obj, ev)
    {
      clickEvent(curView, ev)
      return(TRUE)
    }
  )
      
  gtkAddCallback(drArea(curView), "motion-notify-event",
    function(obj, ev)
    {
      motionEvent(curView, ev)
      return(TRUE)
    }
  )  
}

############
# 5/24/05 add events specifically for graphs
#
# called by initialize method for graphView
############
addEventsforGraphs<-function(curView)
{
  gtkAddCallback(drArea(curView), "button_press_event",
    function(obj, ev)
    {
      clickEvent(curView, ev)
      return(TRUE)
    }
  )
      
  gtkAddCallback(drArea(curView), "motion-notify-event",
    function(obj, ev)
    {
      motionEvent(curView, ev)
      return(TRUE)
    }
  )  
}

############
# 5/24/05 add events for any type of view
# this also adds the view to the viewlist and updates
# the mvclist
#
# called by initialize method for all views
############
addEventsforViews<-function(newView)
{
  dataName<-dataName(newView)

  MVCList<-get("MVCList", mvcEnv)
  allNames<-getModelNames(sort=FALSE)
  mvcindex<-match(dataName, allNames)
  curMVC<-getMVC(dataName)
  controlEnv<-controller(curMVC)

  win<-win(newView)

  assign("activeView", newView, controlEnv)

  # need to add a callback for focus_in_event
  gtkAddCallback(win, "focus_in_event",
    function(obj, ev)
    {
      if (is(newView, "plotView"))
      {
        # set the active device
        dev.set(plotDevice(newView))
      }
      # set the active view
      assign("activeView", newView, controlEnv)
      return(TRUE)
    }
  )

  # need a callback for focus_out_event to remove any open tooltips
  gtkAddCallback(win, "focus_out_event",
    function(obj, ev)
    {
      # no active view
      assign("activeView", c(), controlEnv)
      return(TRUE)
    }
  )

  # add the key press events for pseudo-accelerators
  createKeyPressEvent(win)        

  # add the delete event
  closeWin(newView)

  # add a new sub menu item to the control window
  winCounter<-get("winCounter", mvcEnv)
  assign("winCounter", winCounter+1, mvcEnv)

  curLabel<-paste("Window _", get("winCounter", mvcEnv),
                  "  Ctrl+", get("winCounter", mvcEnv), sep="")
  newMenuItem<-addSubMenuItem(curLabel, "activateWindow", "window",  
                 winNum=get("winCounter", mvcEnv))

  winNum(newView)<-get("winCounter", mvcEnv)

  # set the title of the window
  curTitle<-paste("Window ", get("winCounter", mvcEnv), sep="")
  gtkWindowSetTitle(win, curTitle)

  # add new object to viewList      
  curviewList<-viewList(curMVC)
  viewLen<-length(curviewList)
  curviewList[[viewLen+1]]<-newView
  viewList(curMVC)<-curviewList

  # need to reassign the curMVC to MVCList
  MVCList[[mvcindex]]<-curMVC
  assign("MVCList", MVCList, mvcEnv)

  return(newView)
}

#########
# 5/24/05 create plot of graph
#
# called by initialize method for graphView
#########
graphPlot<-function(curView)
{
  # set a few parameters that don't seem to be set correctly for a gtk device
  par("bg"="transparent")
  par("col"="black")
  par("fg"="black") 

  # need to get layout from the model
  curName<-dataName(curView)
  curModel<-model(getMVC(curName))
  plot(curModel@virtualData)

  # reset the par slot
  plotPar(curView)<-par(no.readonly=TRUE)

  return(curView)
}


#################
# scatterplot is a function to plot a data set
# it is used so that the viewList in viewEnv can be updated
# dfName is the data set name to plot, DFrows are the rows to use
# DFcolumns are the columns to use, dev is the device number
# 
# viewItem is a sPlotView object
#
# called by initialize method for sPlotView
#################
scatterplot<-function(viewItem)
{
  dfName<-dataName(viewItem)
  # these are character so need to match
  DFrows<-dfRows(viewItem)
  DFcolumns<-c(colx(viewItem), coly(viewItem))  
  dev<-plotDevice(viewItem)

  # make sure the length of DFcolumns is 2
  curDF<-getData(modelName=dfName)
  rowIndeces<-match(DFrows,rownames(curDF))
  colIndeces<-match(DFcolumns,colnames(curDF))
  plotDF<-curDF[rowIndeces, colIndeces]

  dev.set(dev)

  curMVC<-getMVC(dfName)
  curvirtualData<-virtualData(model(curMVC))
  if (length(curvirtualData) > 0)
  {
    # get the highlit column
    curHigh<-curvirtualData[rowIndeces, match("highlit", names(curvirtualData))]
    curHide<-curvirtualData[rowIndeces, match("hide", names(curvirtualData))]

    # what about the data stored in the DF columns: color, highlit, pch, hide
    paramList<-setParamList(curvirtualData, rowIndeces, viewItem)  
  }

  # it seems like the window needs to be shown before plotting or I
  # get a gdk error
  win<-win(viewItem)
  win$Show()

  # set a few parameters that don't seem to be set correctly for a gtk device
  par("bg"="transparent")
  par("col"="black")
  par("fg"="black")

  # couldn't put in the parameter list directly 
  # note: may have more than one point here and this can cover up the change
  if (length(curvirtualData) > 0)
    plot(plotDF, col=paramList$col, pch=paramList$pch )
  else
    plot(plotDF)

  if (length(curvirtualData) > 0)
  {
    # need to highlight points
    highPoints<-plotDF[curHigh,]
    if (nrow(highPoints) > 0)
    {
      pointCex<-plotPar(viewItem)$cex
      curCex<-as.integer(pointCex)+1
      points(highPoints[,1], highPoints[,2], cex=curCex, 
           col="red")
    }
 
    # need to hide points
    hidePoints<-plotDF[curHide,]
    if (nrow(hidePoints) > 0)
    {
      backgr<-plotPar(viewItem)$bg
      if (backgr=="transparent")
        backgr="white"
      # make sure to fill in the point as background in case it is colored in
      points(hidePoints[,1], hidePoints[,2], col=backgr, pch=19)
    }

    hideAndHigh<-curHigh & curHide
    if (any(hideAndHigh))
    {
      hideAndHighPoints<-plotDF[hideAndHigh,]
      points(hideAndHighPoints[,1], hideAndHighPoints[,2], cex=curCex,
            col=backgr)
    }
  }

  # need to update plot parameter usr
  plotPar(viewItem)<-par(no.readonly=TRUE)

  return(viewItem)
}

###########
# 9/1/05 create a heatmap view for an exprSet model
#
# called by initialize method for heatmapView
###########
heatmapPlot<-function(viewItem)
{
  dName<-dataName(viewItem)
  curMVC<-getMVC(dName)
  mData<-modelData(model(curMVC))
  vData<-virtualData(model(curMVC))
  
  # check if any genes are hidden
  curHidden<-vData$hide
  if (any(curHidden))
  {
    # then subset the model data
    mData<-mData[-which(curHidden),]
  }

  dev<-plotDevice(viewItem)
  dev.set(dev)
  w<-win(viewItem)
  w$Show()

  par(bg="transparent")
  par(col="black")
  par(fg="black")

  # now plot the heatmap
  retList<-heatmap(exprs(mData))
  ordering(viewItem)<-retList

  plotPar(viewItem)<-par(no.readonly=TRUE)

  return(viewItem)
}

#################
# setParamList is a function to set the parameter list for making a 
# scatterplot
# dataF is the dataframe and dfRows are the dataframe rows
#
# called by scatterplot function
#################
setParamList<-function(dataF,dfRows,plotObject)
{
  colIndex<-match("color", names(dataF))
  curColors<-dataF[dfRows, colIndex]
  pchIndex<-match("pch", names(dataF))
  curPch<-dataF[dfRows, pchIndex]

  hideIndex<-match("hide", names(dataF))
  curHide<-dataF[dfRows, hideIndex]
  highIndex<-match("highlit", names(dataF))
  curHigh<-dataF[dfRows, highIndex]
  
  # make the points that are hidden be the background color
  plotBG<-plotPar(plotObject)$bg
  curColors[curHide]<-plotBG

  return(list(col=curColors, pch=curPch))
}

################
# createGtkDev is a function to create a Gtk device for plotting
# -also adds a callback for the focus event
#
# called by initGraphView and initSPlotView functions
################
createGtkDev<-function(w)
{
  dev <- c()

  w$SetUsize(500, 500)

  # need to add rulers
  vbox<-gtkVBox()
  hrul<-gtkHRuler(show=FALSE)
  hrul$SetMetric("pixels")
  hrul$SetRange(0, 100, 0, 10)
  gtkBoxPackStart(vbox,hrul,expand=FALSE)

  hbox<-gtkHBox()
  gtkBoxPackStart(vbox,hbox)
  vrul<-gtkVRuler(show=FALSE)
  vrul$SetMetric("pixels")
  vrul$SetRange(0, 100, 0, 10)
  gtkBoxPackStart(hbox,vrul,expand=FALSE)

  w$Add(vbox)
 
  drArea <- gtkDrawingArea()
  dev<-addDrawingArea(hbox, drArea, 250, 250, dev, devName="Dev1", 
                      addType=5)

  # want to be informed of these events on the device
  drArea$SetEvents(c("pointer-motion-mask", "pointer-motion-hint-mask", 
                   "button-press-mask", "button-release-mask", 
                   "key-press-mask", "key-release-mask"))

  return(list(win=w, drArea=drArea))
}


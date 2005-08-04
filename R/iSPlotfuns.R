#########
# functions from iSPlot
#########

##################
# identifyPoint determines which point on the scatterplot
# the user clicked (or NULL if no point was clicked)
#
# curplot is the plot information (one element from viewList)
# xyloc is the x and y location that was clicked
##################
identifyPoint<-function(curplot, xyloc)
{
#  print(xyloc)
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
  
#  dfList<-get("dfList",dataEnv)
  curData<-getData(modelName=plotDf)
  rowIndices<-match(plotRows, rownames(curData))
  colxIndex<-match(plotX, colnames(curData))
  colyIndex<-match(plotY, colnames(curData))
#  x<-curData[plotRows, plotX]
#  y<-curData[plotRows, plotY]
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

#############
# setViewDataView allows the user to view the data
#############
setViewDataView<-function()
{
  # get the controller environment for the active MVC
  activeMVC<-get("activeMVC", mvcEnv)
  curMVC<-getMVC(activeMVC)
  controlEnv<-controller(curMVC)

  # reset values before opening new view
  if (length(get("frMVC", mvcEnv)) > 0)
    setEmptyView()       

  tab<-gtkTableNew(4, 4, TRUE)
  vbox<-get("vBox", mvcEnv)
  frMVC<-gtkFrameNew(label=activeMVC)
  frMVC$Add(tab)  

  gtkBoxPackStart(vbox, frMVC)
  assign("frMVC", frMVC, mvcEnv)

#  loadedDF<-getLoadedDF()
  # 7/21/05
  # only allow users to make spreadsheet of the active MVC
  loadedDF<-activeMVC

  frAvail<-gtkFrame("Available Data")
  boxForAvail<-gtkHBox(TRUE, 2)
  sc<-gtkScrolledWindow()
  sc$SetPolicy("automatic", "automatic")
  availDfList<-gtkList()
  if (length(loadedDF) > 0)
    for (i in 1:length(loadedDF))
    {
      availDfListItem<-gtkListItemNewWithLabel(loadedDF[i])
      availDfList$Add(availDfListItem)
    }

  gtkListSetSelectionMode(availDfList, 1)
  sc$AddWithViewport(availDfList)
  boxForAvail$PackStart(sc, expand=TRUE)
  frAvail$Add(boxForAvail)
  gtkTableAttach(tab, frAvail, 0, 2, 0, 4, ypadding=10, xpadding=5)

  gtkAddCallback(availDfList, "select-child",
    function(obj, objchild)
    {
      # need to get the active element in the list
      selItem<-gtkChildren(objchild)[[1]][["label"]]
      assign("showDF", selItem, controlEnv)
    }
  )

  showButton<-gtkButton("Show")
  gtkTableAttach(tab, showButton, 2, 4, 1, 3, ypadding=50, xpadding=70)

  gtkAddCallback(showButton, "clicked",
    function(obj)
    {
      showDF<-get("showDF", controlEnv)

      # need to also check that this data is not currently being shown in
      # a spreadsheet
      if (showDF != "")
      {
        curMVC<-getMVC(showDF)
        curviewList<-viewList(curMVC)
        booSpreadView<-unlist(lapply(curviewList, function(x) {is(x, 
                                 "spreadView")}))
        # if there are any spread views then this data set is already shown
        # in a spreadsheet
        if (any(booSpreadView))
          booCreate<-FALSE
        else
          booCreate<-TRUE

        if (booCreate)
        {
          # create and handle a message to create a view
          vMessage<-new("gAddViewMessage", dataName=showDF, type="spreadView")
          handleMessage(vMessage)
        }
      }
    }
  )
}

##########
# different from the iSPlot version
##########
getLoadedDF<-function()
{
  mNames<-NULL
  MVCList<-get("MVCList", mvcEnv)
  if (length(MVCList) > 0)
  {
    modelList<-lapply(MVCList, model)
    # need to check the class of model
    booDF<-unlist(lapply(modelList, testDfClass))
    if (any(booDF))
    {
      dfModels<-modelList[booDF]
      mNames<-sort(unlist(lapply(dfModels, modelName)))
    }
  }
  return(mNames)
}

########
# check if x is of class dfModel
########
testDfClass<-function(x)
{
  is(x, "dfModel")
}

##############
# setPlotDView sets the control window so it
# shows the view of loaded dataframes to
# choose which one to plot
##############
setPlotDView<-function()
{
  # get the controller environment for the active MVC
  activeMVC<-get("activeMVC", mvcEnv)
  curMVC<-getMVC(activeMVC)
  controlEnv<-controller(curMVC)

  # reset values before opening new view
  if (length(get("frMVC", mvcEnv)) > 0)
    setEmptyView()

  tab<-gtkTableNew(8, 8, TRUE)
  vbox<-get("vBox", mvcEnv)
  frMVC<-gtkFrameNew(label=activeMVC)
  frMVC$Add(tab)  

  gtkBoxPackStart(vbox, frMVC)
  assign("frMVC", frMVC, mvcEnv)

#  loadedDF<-getLoadedDF()
  # changed 7/21/05
  # only allow users to plot the active MVC
  loadedDF<-activeMVC

  frLoaded<-gtkFrame("Loaded Data")
  boxForLoaded<-gtkVBox(TRUE, 2)
  sc<-gtkScrolledWindow()
  sc$SetPolicy("automatic", "automatic")
  loadedDfList<-gtkList()
  if (length(loadedDF)>0)
    for (i in 1:length(loadedDF))
    {
      loadedDfListItem<-gtkListItemNewWithLabel(loadedDF[i])
      loadedDfList$Add(loadedDfListItem)
    }

  gtkListSetSelectionMode(loadedDfList, 1)
  sc$AddWithViewport(loadedDfList)
  boxForLoaded$PackStart(sc, expand=TRUE)
  frLoaded$Add(boxForLoaded)
  gtkTableAttach(tab, frLoaded, 0, 4, 0, 6, xpadding=10)

  frVar<-gtkFrame("Data Variables")
  gtkTableAttach(tab, frVar, 4, 8, 0, 6, xpadding=10)

  gtkAddCallback(loadedDfList, "select-child",
    function(obj, objchild)
    {
      # reset chosen X and Y because now looking at new dataframe
      assign("chosenX", "", controlEnv)
      assign("chosenY", "", controlEnv)

      # need to the active element in the list
      selItem<-gtkChildren(objchild)[[1]][["label"]]
      assign("dataF", selItem, controlEnv)

      setVariables(selItem, frVar)
    }
  )

  plotBut<-gtkButton("Plot")
  gtkTableAttach(tab, plotBut, 3, 5, 6, 8, xpadding=20, ypadding=20)

  gtkAddCallback(plotBut, "clicked",
    function(obj, ev)
    {
      # need to get the two plot variables and call plotting function
      chosenX<-get("chosenX", controlEnv)
      chosenY<-get("chosenY", controlEnv)
      if (chosenX != "" && chosenY != "")
      {
        # then need to call plotting function
        dataF<-get("dataF", controlEnv)
        curDF<-getData(modelName=dataF)

        DFrows<-rownames(curDF)
#        DFcolumns<-c(chosenX, chosenY)

        # 5/24/05 new message method
        vMessage<-new("gAddViewMessage", dataName=dataF, type="sPlotView",
                      dfRows=DFrows, colx=chosenX, coly=chosenY)
#        print(vMessage)
        handleMessage(vMessage)
      }
    }
  )
}

##########
# setVariables sets the dataframe variables in a list box
#
# dataF is the dataframe name, frVar is the frame that
# will hold the dataframe variables
##########
setVariables<-function(modelName, frVar)
{
  # get the controller environment for the current model
  curMVC<-getMVC(modelName)
  controlEnv<-controller(curMVC)

  # need to reset the button boxes before adding anything
  resetVarButtonBoxes(frVar)

  # need to add the button boxes along with the variable labels
  varNames<-names(getData(modelName))

  boxForVar<-gtkVBox(TRUE, 2)
  sc<-gtkScrolledWindow()
  sc$SetPolicy("automatic", "automatic")

  XbuttonList<-list()
  YbuttonList<-list()

  if (length(varNames) > 0)
  {
    # length(varNames)=# of rows, 3 columns, homogenous=FALSE??
    tab<-gtkTable(length(varNames), 3, FALSE)

    for (i in 1:length(varNames))
    {
      XbuttonList[[i]]<-gtkToggleButton("X")
#      XbuttonList[[i]]$SetUsize(10, 10)
      YbuttonList[[i]]<-gtkToggleButton("Y")
#      YbuttonList[[i]]$SetUsize(10, 10)
      lab<-gtkLabel(varNames[i])
#      lab$SetUsize(10, 10)
      gtkTableAttach(tab, XbuttonList[[i]], 0, 1, i-1, i, xpadding=5)
      gtkTableAttach(tab, YbuttonList[[i]], 1, 2, i-1, i, xpadding=5)
      gtkTableAttach(tab, lab, 2, 3, i-1, i, xpadding=5)

      XbuttonList[[i]]$AddCallback("clicked",
        substitute(setToggleX(j, dataFr), list(j=i, dataFr=modelName)))
      YbuttonList[[i]]$AddCallback("clicked",
        substitute(setToggleY(j, dataFr),list(j=i, dataFr=modelName)))
    }
    sc$AddWithViewport(tab)
  }
  assign("toggleButtonXList", XbuttonList, controlEnv)
  assign("toggleButtonYList", YbuttonList, controlEnv)

  boxForVar$PackStart(sc, expand=FALSE)

  frVar$Add(boxForVar)
}

###########
# resetVarButtonBoxes removes all children from the frVar
#
# frVar is the frame containing the button boxes for the
# dataframe variables
###########
resetVarButtonBoxes<-function(frVar)
{
  # remove all items from the frame
  kids<-gtkContainerGetChildren(frVar)
  if (length(kids) > 0)
    for (i in 1:length(kids))
    {
      gtkWidgetDestroy(kids[[i]])
    }
}

######
# make an interface for getting data, in case new
# data types are added besides dataframe or matrix
######
getData<-function(modelName)
{
  curMVC<-getMVC(modelName)
  curModel<-model(curMVC)
  curData<-modelData(curModel)

  return(curData)
}

############
# setToggleX sets the chosen X value
#
# j is the index of the button that just changed state
# dataFr is the current dataframe
############
setToggleX<-function(j, dataFr)
{
  setToggleInactive(j, "X", dataFr)
}

##############
# setToggleY sets the chosen Y value
#
# j is the index of the button that just changed state
# dataFr is the current dataframe
##############
setToggleY<-function(j, dataFr)
{
  setToggleInactive(j, "Y", dataFr)
}

##############
# setToggleInactive sets the toggle buttons so
# only one button can be active at a time and it
# also sets the chosenX/chosenY values for plotting
#
# j is the index of the button that just changed state
# type is X or Y
# dataFr is the current dataframe
##############
setToggleInactive<-function(j, type, dataFr)
{
  # get the controller environment for the current model
  curMVC<-getMVC(dataFr)
  controlEnv<-controller(curMVC)  

  # need to check if there are two buttons active
  XbuttonList<-get("toggleButtonXList", controlEnv)
  YbuttonList<-get("toggleButtonYList", controlEnv)

  indexActive<-0
  numActive<-0
  for (i in 1:length(XbuttonList))
  {
    if (type=="X")
    {
      if (gtkToggleButtonGetActive(XbuttonList[[i]])==TRUE)
      {
        numActive<-numActive+1
        indexActive<-i
      }
    }
    else
    {
      if (gtkToggleButtonGetActive(YbuttonList[[i]])==TRUE)
      {
        numActive<-numActive+1
        indexActive<-i
      }
    }
  }

  if (numActive > 1)
    for (i in 1:length(XbuttonList))
    {
      if (i != j)
      {
        if (type=="X")
        {
          gtkToggleButtonSetActive(XbuttonList[[i]], FALSE)
        }
        else
        {
          gtkToggleButtonSetActive(YbuttonList[[i]], FALSE)
        }
      }
    }

  # if there is only one active toggle button - set that to be chosen
  else
  {
    # get the variable names
    varNames<-names(getData(dataFr))

    if (type=="X")
      assign("chosenX", varNames[indexActive], controlEnv)
    else
      assign("chosenY", varNames[indexActive], controlEnv)
  }
}


#########
# 5/24/05 create a graph view through initialize method
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
#  nodeShape<-"ellipse"

  # the graph data
  actGraph<-getData(modelName=dataName)
  Sys.sleep(0.5)

#  curlayout<-agopen(actGraph, name=dataName, nodeAttrs=makeNodeAttrs(actGraph, 
#                shape=nodeShape, width=3))
  # for now make the labels on the nodes blank (because having trouble
  # plotting them)
  if (is.null(virData))
  {
#    print(dataName)
    curlayout<-agopen(actGraph, name=dataName, nodeAttrs=makeNodeAttrs
               (actGraph, label="", shape=nodeShape, fillcolor="transparent"))
  }
  else
  {
    # use the fillcolors that are already in the virtualData slot
    # not sure if other view info should be used (in addition to fillcolor)
    curFillColors<-unlist(lapply(AgNode(virData), fillcolor))
    print(curFillColors)
    curlayout<-agopen(actGraph, name=dataName, nodeAttrs=makeNodeAttrs
               (actGraph, label="", shape=nodeShape, fillcolor=curFillColors))
  }

  newRetList<-list(win=win, drArea=drArea, plotDevice=dev.cur(),
                   plotPar=par(no.readonly=TRUE), graphLayout=curlayout)
  return(newRetList)
}

#########
# 5/24/05 create a scatterplot view through initialize method
# here just setting up the device
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
    }
  )
   
  gtkAddCallback(curclist, "unselect-row",
    function(obj, row, col, ev)
    {
      # need this variable to make sure don't end up in an infinite loop
      if (get("contLoop", controlEnv)==FALSE)
        clickEvent(curView, (row+1), event="unselect")
    }
  )
}

##########
# 5/24/05 add events specifically for scatterplots
##########
addEventsforSPlots<-function(curView)
{
  gtkAddCallback(drArea(curView), "button_press_event",
    function(obj, ev)
    {
      clickEvent(curView, ev)
    }
  )
      
  gtkAddCallback(drArea(curView), "motion-notify-event",
    function(obj, ev)
    {
      motionEvent(curView, ev)
    }
  )  
}

############
# 5/24/05 add events specifically for graphs
############
addEventsforGraphs<-function(curView)
{
  gtkAddCallback(drArea(curView), "button_press_event",
    function(obj, ev)
    {
      clickEvent(curView, ev)
    }
  )
      
  gtkAddCallback(drArea(curView), "motion-notify-event",
    function(obj, ev)
    {
      motionEvent(curView, ev)
    }
  )  
}

############
# 5/24/05 add events for any type of view
# this also adds the view to the viewlist and updates
# the mvclist
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
    }
  )

  # need a callback for focus_out_event to remove any open tooltips
  gtkAddCallback(win, "focus_out_event",
    function(obj, ev)
    {
      # no active view
      assign("activeView", c(), controlEnv)
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
#  plot(curView@graphLayout)

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
#  plot(plotDF, col=paramList$col, pch=paramList$pch)
  if (length(curvirtualData) > 0)
    plot(plotDF, col=paramList$col, pch=paramList$pch )
  else
    plot(plotDF)
#  print(plotDF)
#  print(paramList$col)
#  print(paramList$pch)

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

#################
# setParamList is a function to set the parameter list for making a 
# scatterplot
# dataF is the dataframe and dfRows are the dataframe rows
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

###########
# checkPoint sees if the x,y values fall in a point from the dataframe
# only called when the viewMode is identify
###########
checkPoint<-function(curx, cury, curPlot)
{
  # expecting this to be a plot
  activeView<-get("activeView", viewEnv) 
#  print(activeView) 

  # set the user parameter
  curPar<-plotPar(curPlot)
  par("usr"=curPar$usr)

  curbg<-plotPar(curPlot)$bg
  if (curbg=="transparent")
    curbg<-"white"

  if (is(activeView, "plotView"))
  {
    xyloc<-list(x=curx, y=cury)

    # convert to user coordinates
    xyInches<-pix2inches(curx, cury)
    xyUsr<-inches2usr(xyInches$x, xyInches$y)
 
    curpt<-identifyPoint(curPlot, xyUsr)

    if (!is.null(curpt))
    {
      # need to keep information about the current and old points
      curPoint<-get("curIDpoint", viewEnv)

      continue<-FALSE
      if (length(curPoint$closestPoint) != length(curpt$closestPoint))
        continue<-TRUE
      else
        if (any(curPoint$closestPoint != curpt$closestPoint))
          continue<-TRUE
      if (length(curPoint)==0)
        continue<-TRUE

      if (continue)
      {
        # remove old point highlight before adding new text
        if (length(curPoint) > 0)
        {
          dfMessage<-new("gUpdateDataMessage", from=curPlot, where=curPoint)
          handleMessage(dfMessage)

          printText(curPoint,curPlot,curbg)
        }
        # now update new point
        assign("curIDpoint",curpt,viewEnv)    

        dfMessage<-new("gUpdateDataMessage", from=curPlot, where=curpt)

        # now need to handle the message
        handleMessage(dfMessage)      

        printText(curpt,curPlot,get("highColor",dataEnv))
      }
    }

    else
    {
      curPoint<-get("curIDpoint",viewEnv)
      # also need to remove old point highlight
      if (length(curPoint) > 0)
      {
        dfMessage<-new("gUpdateDataMessage", from=curPlot, where=curPoint)
        handleMessage(dfMessage)

        assign("curIDpoint",list(),viewEnv)
        printText(curPoint,curPlot,curbg)
      }
    }
  }
}


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
identifyPoint<-function(curplot,xyloc)
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
  

  dfList<-get("dfList",dataEnv)
  curData<-getData(type="dataframe",name=plotDf)
  x<-curData[plotRows,plotX]
  y<-curData[plotRows,plotY]

  # check if x or y is a factor
  if (is(x,"factor"))
    x<-as.numeric(x)
  if (is(y,"factor"))
    y<-as.numeric(y)

  rowNames<-row.names(curData)

  # now need to identify the closest point
  totdiff<-(xyloc$x-x)^2+(xyloc$y-y)^2
  booClosest<-min(totdiff)==totdiff
  
  closestPoint<-rowNames[booClosest]
  closestXY<-list(x=x[booClosest],y=y[booClosest])
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
    return(list(closestXY=closestXY,closestPoint=closestPoint))
  else
    return(NULL)
}

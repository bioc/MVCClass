
##############
# create a class for model objects
##############
# a virtual model class that all model classes will be derived from
# modelData will be the actual model data, linkData will be any data
# that links the model to its parent model (a list of 2 functions:
# linkToParent and linkToChild), virtualData is any data that
# is needed for views of the model, and modelName is the name of the model
# (a way for the user to refer to the data)
setClass("gModel", representation(modelData="ANY", linkData="list", 
         virtualData="ANY", modelName="character"), contains=("VIRTUAL"))

# couldn't get RagraphNULL to pass the installation - didn't recognize
# Ragraph class
#setClassUnion("RagraphNULL", members=c("Ragraph", "NULL"))

# for a model that has graph data
# initially the virtualData slot for graphModel will be NULL (until a 
# view is created)
#setClass("graphModel", representation(modelData="graph", 
#                       virtualData="RagraphNULL"), contains="gModel")
setClass("graphModel", representation(modelData="graph"), contains="gModel")

# for a model that has expression data - 
# the data list should include an exprSet object and maybe a vector of LL ids
setClass("exprModel", representation(modelData="exprSet"), contains="gModel")
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

######
# initialize methods
######

setMethod("initialize", "exprModel", 
  function(.Object, modelData, modelName, linkData=list(), virtualData=NULL)
  {
    if (is.null(virtualData))
    {
      # need to create the virtualData slot - will be the same as the
      # virtualData slot for a dfModel
      defaultPlotData<-list(color="black", pch=1, highlit=FALSE, hide=FALSE)
      numRows<-nrow(exprs(modelData))
      if (length(defaultPlotData) > 0)
      {
        virData<-data.frame(rep(defaultPlotData[[1]], numRows), 
                            row.names=rownames(exprs(modelData)))
        virData[,1]<-as.character(virData[,1])
        colnames(virData)<-names(defaultPlotData)[1]
      }
      if (length(defaultPlotData) > 1)
      {
        for (i in 2:length(defaultPlotData))
        {
          virData[,i]<-rep(defaultPlotData[[i]], numRows)
        }
        colnames(virData)<-names(defaultPlotData)
      }
    }
    else
      virData<-virtualData

    # now assign values to the object and then return object
    .Object@modelData<-modelData
    .Object@linkData<-linkData
    .Object@virtualData<-virData
    .Object@modelName<-modelName
    .Object
  }
)

#######
#
#######
setMethod("initialize", "dfModel",
  function(.Object, modelData, modelName, linkData=list(), virtualData=NULL)
  {
    # need to check the class of the model data
    if (is(modelData, "matrix"))
      modelData<-as.data.frame(modelData)

    if (is.null(virtualData))
    {
      # need to create the virtualData slot 
      defaultPlotData<-list(color="black", pch=1, highlit=FALSE, hide=FALSE)
      numRows<-nrow(modelData)
      if (length(defaultPlotData) > 0)
      {
        virData<-data.frame(rep(defaultPlotData[[1]], numRows), 
                            row.names=rownames(modelData))
        virData[,1]<-as.character(virData[,1])
        colnames(virData)<-names(defaultPlotData)[1]
      }
      if (length(defaultPlotData) > 1)
      {
        for (i in 2:length(defaultPlotData))
        {
          virData[,i]<-rep(defaultPlotData[[i]], numRows)
        }
        colnames(virData)<-names(defaultPlotData)
      }
    }
    else
      virData<-virtualData
    # now assign values to the object and then return object
    .Object@modelData<-modelData
    .Object@linkData<-linkData
    .Object@virtualData<-virData
    .Object@modelName<-modelName
    .Object
  }
)

##########
# create a method to update a model
##########
if (is.null(getGeneric("updateModel")))
  setGeneric("updateModel", function(object, type, data)
            standardGeneric("updateModel"))

###########
# 8/31/05 need to determine how the update model method will work for 
# an exprSet model
#
# for the virtualData slot for an exprModel is the same as for a dfModel
###########
setMethod("updateModel", "exprModel",
  function(object, type, data)
  {
    # for a dataframe want to change the virtualData slot
    virData<-virtualData(object)
    dataName<-modelName(object)

    colIndex<-match(type, colnames(virData))
    rowName<-unique(names(data))
    rowIndex<-match(rowName, rownames(virData))

    oldValue<-virData[rowIndex, colIndex]
    # update the value
    virData[rowIndex, colIndex]<-unlist(data)

    # now update the objects
    virtualData(object)<-virData

    # need to update the MVC object - may not be the active MVC
    curMVC<-getMVC(dataName)
    model(curMVC)<-object

    # will also need to update the MVCList
    mvcList <- get("MVCList", mvcEnv)
    allNames <- getModelNames(sort = FALSE)

    index<-match(dataName, allNames)
    mvcList[[index]]<-curMVC
    assign("MVCList", mvcList, mvcEnv)

    # return the data needed to update the views
    # need the old value to remove the old point from the plot before
    # re-adding the point with the new value
    viewData<-list(colName=type, rowName=rowName, oldValue=oldValue, 
                   newValue=unlist(data))
    return(viewData)
  }
)

setMethod("updateModel", "dfModel",
  function(object, type, data)
  {
    # for a dataframe want to change the virtualData slot
    virData<-virtualData(object)
    dataName<-modelName(object)

    colIndex<-match(type, colnames(virData))
    rowName<-unique(names(data))
    rowIndex<-match(rowName, rownames(virData))

    oldValue<-virData[rowIndex, colIndex]
    # update the value
    virData[rowIndex, colIndex]<-unlist(data)

    # now update the objects
    virtualData(object)<-virData

    # need to update the MVC object - may not be the active MVC
    curMVC<-getMVC(dataName)
    model(curMVC)<-object

    # will also need to update the MVCList
    mvcList <- get("MVCList", mvcEnv)
    allNames <- getModelNames(sort = FALSE)

    index<-match(dataName, allNames)
    mvcList[[index]]<-curMVC
    assign("MVCList", mvcList, mvcEnv)

    # return the data needed to update the views
    # need the old value to remove the old point from the plot before
    # re-adding the point with the new value
    viewData<-list(colName=type, rowName=rowName, oldValue=oldValue, 
                   newValue=unlist(data))
    return(viewData)
  }
)

########
# 7/29/05 update the virtualData slot in the model due to a change
# in the node values (in the future may want to update edges)
########
setMethod("updateModel", "graphModel",
  function(object, type, data)
  {
    nameType<-names(type)
    if (nameType=="node") 
    {
      # if data is only one AgNode, then make it a list with one element
      if (!is(data, "list"))
      {
        data<-list(data)
        # this will give the node name
        names(data)<-name(data)
      }
      dataName<-modelName(object)
      curMVC<-getMVC(dataName)
      # need to update the virtual data slot
      virData<-virtualData(object)
      curNodes<-AgNode(virData)
      allNodeNames<-unlist(lapply(curNodes, name))

      for (i in 1:length(data))
      {
        # data will be the new node(s)
        newNode<-data[[i]]
        curName<-name(newNode)
        curIndex<-match(curName, allNodeNames)

        # update the nodes
        curNodes[[curIndex]]<-newNode
      }

      # now update the model
      AgNode(virData)<-curNodes
      virtualData(object)<-virData
      model(curMVC)<-object      

      # need to update the MVC list
      mvcList <- get("MVCList", mvcEnv)
      allModelNames <- getModelNames(sort = FALSE)
      index<-match(dataName, allModelNames)
      mvcList[[index]]<-curMVC
      assign("MVCList", mvcList, mvcEnv)

      # need to return a list, but data is already a list of AgNode(s)
      return(data)
    }
    # for the future
    if (nameType=="edge")
    {
    }
  }
)


mvcEnv<-new.env(parent=.GlobalEnv)

assign("defaultPlotData", list(color="black", pch=1, highlit=FALSE, 
        hide=FALSE), mvcEnv)
assign("MVCList", list(), mvcEnv)

# I don't know any other way to know what methods each class of model has
assign("modelMethods", list(dfModel=c("createSubsetModel"),
        exprModel=c("createGOmodel"), graphModel=c()), mvcEnv)

##############
# create a class for model objects
##############
# a virtual model class that all model classes will be derived from
# modelData will be the actual model data, linkData will be any data
# that links the model to its parent model, virtualData is any data that
# is needed for views of the model, and modelName is the name of the model
# (a way for the user to refer to the data)
setClass("gModel", representation(modelData="ANY", linkData="ANY", 
         virtualData="ANY", modelName="character"), contains=("VIRTUAL"))

# for a model that has graph data
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

######
# initialize methods
######

#setMethod("initialize", "exprModel", 
#  function(.Object, data, name, linkData=NULL)
#  {
#    getChipType()
#
#    curChip<-get("chipType", controlEnv)
#    library(curChip, character.only=TRUE)
#    affyids<-rownames(exprs(data))
#    curEnv<-paste(curChip, "LOCUSID", sep="")
#
#    # get the locus link ids (the link data???)
#    llids<-mget(affyids, eval(as.name(curEnv)))
#    .Object@modelData<-data
#    .Object@linkData<-llids
#    .Object@modelName<-name
#    .Object
#  }
#)

setMethod("initialize", "dfModel",
  function(.Object, mData, mName, linkData=NULL)
  {
    # need to create the virtualData slot 
    defaultPlotData<-get("defaultPlotData", mvcEnv)
    
    numRows<-nrow(mData)
    if (length(defaultPlotData) > 0)
    {
      virData<-data.frame(rep(defaultPlotData[[1]], numRows), 
                          row.names=row.names(mData))
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
    
    # now assign values to the object and then return object
    .Object@modelData<-mData
    .Object@linkData<-linkData
    .Object@virtualData<-virData
    .Object@modelName<-mName
    .Object
  }
)

##########
# create a method to derive a new model
##########

if (is.null(getGeneric("createGOmodel")))
  setGeneric("createGOmodel", function(object, Ontology="MF")
            standardGeneric("createGOmodel"))
setMethod("createGOmodel", "exprModel", function(object, Ontology="MF")
  {
    # need to load the GOstats library
    require(GOstats) || stop("Must have the GOstats library")
    # need to get the locus link ids from the linkData slot
    llids<-unique(as.character(unlist(linkData(object))))
    # now create the graph
    curGraph<-makeGOGraph(llids)

    # next need to create the link data
    
    # from makeGOGraph function
    newGOids <- mget(llids, env = GOLOCUSID2GO, ifnotfound = NA)
    if (length(newGOids) == 1)
      bd <- is.na(newGOids[[1]])
    else 
      bd <- is.na(newGOids)
    newGOids <- newGOids[!bd]

    newGOids <- lapply(newGOids, function(x) x[sapply(x, function(x) {
        if (is.na(x$Ontology))
            return(FALSE)
        else x$Ontology == Ontology
    })])

    # remove any LLids from list that have no GO ids in this ontology
    GOlen<-unlist(lapply(newGOids, length))
    newGOids<-newGOids[-which(GOlen==0)]

    # this will give a list with LLids as names and GO terms as the elements
    LLtoGO<-lapply(newGOids, names)

    # now need to look at the parent GO terms for each element in LLtoGO
    for (i in 1:length(LLtoGO))
    {
      
    }
    
  }
)

############
# this will take a data frame model and create a new data frame model
# that is a subset of the original model
# the subset will be determined by the rownames and colnames
# mName is the name of the new model
############
if (is.null(getGeneric("createSubsetModel")))
  setGeneric("createSubsetModel", function(object, mName, subsetData)
            standardGeneric("createSubsetModel"))

# expect subsetData for dfModel to be a list with elements rNames and
# cNames (for row names and column names) 
setMethod("createSubsetModel", "dfModel", 
  function(object, mName, subsetData)
  {
    rNames<-subsetData$rNames
    cNames<-subsetData$cNames

    # will need to update modelData 
    mData<-modelData(object)

    # if the rownames or colnames variable is empty then assume the user
    # doesn't want to subset that dimension
    if (length(rNames) == 0)
      rNames<-row.names(mData)
    if (length(cNames) == 0)
      cNames<-colnames(mData)

    matchCIndex<-match(cNames, colnames(mData))
    matchRIndex<-match(rNames, row.names(mData))

    newmData<-mData[matchRIndex, matchCIndex]

    # need to create the link data
    # the keys in the data frame model are the row names
    # so make a dataframe linking the keys between the two models
    lData<-data.frame(cbind(rNames, rNames))
    colnames(lData)<-c("parentKey", "childKey")

    # this function is in iSNetwork in winControl.R!!!!!!!!
    loadModel(data=newmData, type="data.frame", name=mName, linkData=lData) 
#    newModel<-new("dfModel", mData=newmData, mName=mName, linkData=lData)
#    return(newModel)
  }
)


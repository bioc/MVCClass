###########
# create a class for messages
###########
# a virtual class for message
setClass("gMessage")

setClass("gUpdateMessage", representation(type="character", mData="list",
          dataName="character"), contains="gMessage")

setClass("gUpdateViewMessage", contains="gUpdateMessage")

# 9/5/05 added the 'from' slot so we can loop through all linked models 
# when updating the data (without storing a global variable to ensure
# that the loop is not infinite)
setClass("gUpdateDataMessage", representation(from="character"), 
          contains="gUpdateMessage")

setClass("gAddMessage", representation(dataName="character", mData="list",
         type="character"), contains="gMessage")
setClass("gAddViewMessage", contains="gAddMessage")
setClass("gAddDataMessage", contains="gAddMessage")

# added 7/13/05 to create a child model
setClass("gAddChildMessage", contains="gAddMessage")

# added 7/14/05 to update a parent model
# the slot for this class tells the parent what the child update data
# message was
setClass("gUpdateParentMessage",
         representation(childUpdateDataMessage="gUpdateDataMessage"), 
         contains="gMessage")
# added 7/19/05 to update a child model
# the slot for this class tells the child what the parent update data
# message was
setClass("gUpdateChildMessage",
         representation(parentUpdateDataMessage="gUpdateDataMessage",
         childName="character"),
         contains="gMessage")

#####
# accessor functions
#####
if (is.null(getGeneric("from")))
  setGeneric("from", function(object)
            standardGeneric("from"))
setMethod("from", "gUpdateDataMessage", function(object)
         object@from)

if (is.null(getGeneric("childUpdateDataMessage")))
  setGeneric("childUpdateDataMessage", function(object)
            standardGeneric("childUpdateDataMessage"))
setMethod("childUpdateDataMessage", "gUpdateParentMessage", function(object)
         object@childUpdateDataMessage)

if (is.null(getGeneric("parentUpdateDataMessage")))
  setGeneric("parentUpdateDataMessage", function(object)
            standardGeneric("parentUpdateDataMessage"))
setMethod("parentUpdateDataMessage", "gUpdateChildMessage", function(object)
         object@parentUpdateDataMessage)

if (is.null(getGeneric("childName")))
  setGeneric("childName", function(object)
            standardGeneric("childName"))
setMethod("childName", "gUpdateChildMessage", function(object)
         object@childName)

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
setMethod("dataName", "gUpdateMessage", function(object)
         object@dataName)

#####
# setting the slots
#####
if (is.null(getGeneric("from<-")))
  setGeneric("from<-", function(object, value)
            standardGeneric("from<-"))
setReplaceMethod("from", "gUpdateDataMessage", function(object, value)
         {
           object@from<-value
           object
         }
)

if (is.null(getGeneric("childUpdateDataMessage<-")))
  setGeneric("childUpdateDataMessage<-", function(object, value)
            standardGeneric("childUpdateDataMessage<-"))
setReplaceMethod("childUpdateDataMessage", "gUpdateParentMessage", function(object, value)
         {
           object@childUpdateDataMessage<-value
           object
         }
)

if (is.null(getGeneric("parentUpdateDataMessage<-")))
  setGeneric("parentUpdateDataMessage<-", function(object, value)
            standardGeneric("parentUpdateDataMessage<-"))
setReplaceMethod("parentUpdateDataMessage", "gUpdateChildMessage", function(object, value)
         {
           object@parentUpdateDataMessage<-value
           object
         }
)

if (is.null(getGeneric("childName<-")))
  setGeneric("childName<-", function(object, value)
            standardGeneric("childName<-"))
setReplaceMethod("childName", "gUpdateChildMessage", function(object, value)
         {
           object@childName<-value
           object
         }
)

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
setReplaceMethod("dataName", "gUpdateMessage", function(object, value)
         {
           object@dataName<-value
           object
         }
)

#####
# initialize methods
#####
# added 7/20/05
setMethod("initialize", "gUpdateChildMessage",
  function(.Object, parentUpdateDataMessage, childName)
  {
    .Object@parentUpdateDataMessage<-parentUpdateDataMessage
    .Object@childName<-childName
    .Object
  }
)

# added 7/14/05
setMethod("initialize", "gUpdateParentMessage",
  function(.Object, childUpdateDataMessage)
  {
    .Object@childUpdateDataMessage<-childUpdateDataMessage
    .Object
  }
)

setMethod("initialize", "gUpdateViewMessage", 
  function(.Object, type, mData, dataName="")
  {
    .Object@type<-type
    .Object@mData<-mData
    if (dataName=="")
      dataName<-get("activeMVC", mvcEnv)
    .Object@dataName<-dataName

    .Object
  }
)

#####
# update the data stored in the modelData slot of a model
#####
setMethod("initialize", "gUpdateDataMessage", 
  function(.Object, type, mData, dataName="", from="")
  {
    .Object@type<-type
    .Object@mData<-mData
    if (dataName=="")
      dataName<-get("activeMVC", mvcEnv)
    if (from=="")
      from<-dataName
    .Object@dataName<-dataName
    .Object@from<-from

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

# added 7/13/05 
setMethod("initialize", "gAddChildMessage",
  function(.Object, data, dataName, type)
  {
    .Object@dataName<-dataName
    .Object@mData<-data
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
setMethod("handleMessage", "gUpdateChildMessage",
  function(object, ...)
  {
    curchildName<-childName(object)
   
    curMVC<-getMVC(curchildName)
    linkData<-linkData(model(curMVC))
    linkToChild<-linkData$linkToChild
    
    # send linkToChild the gUpdateChildMessage object so linkToChild will
    # get both the gUpdateDataMessage from the parent MVC and it will get
    # the current childName
    newUpdateMessage<-linkToChild(object)
    # there may be no update message for the child if the child does
    # not contain the data that changed in the parent (i.e. the child is a
    # subset of the parent)
    if (!is.null(newUpdateMessage))
    {
      # set the from slot so the update data message knows that this message
      # came from the parent
      from(newUpdateMessage)<-dataName(parentUpdateDataMessage(object))
      handleMessage(newUpdateMessage)
    }
  }
)

setMethod("handleMessage", "gUpdateParentMessage",
  function(object, ...)
  {
    # need to get the linkToParent function, which is stored in the linkData
    # slot in the model object
    childName<-dataName(childUpdateDataMessage(object))
    curMVC<-getMVC(childName)

    linkData<-linkData(model(curMVC))
    # linkData is a list with 2 elements: linkToParent and linkToChild
    linkToParent<-linkData$linkToParent

    # send linkToParent the gUpdateParentMessage object, which will contain
    # the gUpdateDataMessage for the child MVC
    # newUpdateMessage will be the gUpdateDataMessage for the parent
    newUpdateMessage<-linkToParent(object)
    # set the from slot so the update data message knows that this message
    # came from a child
    from(newUpdateMessage)<-dataName(childUpdateDataMessage(object))
    handleMessage(newUpdateMessage)
  }
)

setMethod("handleMessage", "gUpdateViewMessage",
  function(object, ...)
  {
    type<-type(object)
    viewdata<-mData(object)
    dataName<-dataName(object)

    # now need to update the view - note it may not be the active MVC
    curMVC<-getMVC(dataName)
    curVList<-viewList(curMVC)
    controlEnv<-controller(curMVC)

    if (length(dev.list()) > 0)
      activeDev<-dev.cur()

    assign("contLoop", TRUE, controlEnv)
    controller(curMVC)<-controlEnv
    MVCList<-get("MVCList", mvcEnv)
    allNames <- getModelNames(sort = FALSE)

    index<-match(dataName, allNames)
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
        redrawView(curVList[[i]])
      }
    }

    # need to get this again because the MVC object has changed
    curMVC<-getMVC(dataName)
    curVList<-viewList(curMVC)
    controlEnv<-controller(curMVC)

    assign("contLoop", FALSE, controlEnv)
    controller(curMVC)<-controlEnv
    MVCList<-get("MVCList", mvcEnv)
    allNames <- getModelNames(sort = FALSE)

    index<-match(dataName, allNames)
    MVCList[[index]]<-curMVC
    assign("MVCList", MVCList, mvcEnv)

    # reset the active device
    if (length(dev.list()) > 0)
      dev.set(activeDev)
  }
)

########
# added on 7/14/05 so that we also update parent/children of the model
########
setMethod("handleMessage", "gUpdateDataMessage",
  function(object,...)
  {
    data<-mData(object)
    type<-type(object)

    dataName<-dataName(object)
    from<-from(object)

    # now need to update the model
    curMVC<-getMVC(dataName)
    curModel<-model(curMVC)
    # will get an error here if print statement is used when the model to
    # be updated is a graph

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
      uvMessage<-new("gUpdateViewMessage", type="updateView", mData=viewdata,
                      dataName=dataName)
#      print(uvMessage)
      handleMessage(uvMessage)
    }

    # after updating its views, now need to look for parent and children
    parentName<-parentMVC(curMVC)
    childrenName<-childMVCList(curMVC)

    # only send the message if there is a parent model and if that parent 
    # model is not where the update data message came from
    if (!(parentName %in% c("", from)))
    {
      sendToParent<-get("sendToParent", controller(curMVC))
      if (sendToParent==TRUE)
      {
        # parentMessage will be a new gUpdateDataMessage
        parentMessage<-new("gUpdateParentMessage", 
                           childUpdateDataMessage=object)
        handleMessage(parentMessage)
      }
    }
    if (length(childrenName) > 0)
    {
      sendToChild<-get("sendToChild", controller(curMVC))
      # can have multiple children (can only have one parent)
      for (i in 1:length(childrenName))
      {
        # only pass the message on to the children if there are children
        # and the child is not where the update data message came from
        if (!(childrenName[[i]] %in% c("", from)))
        {
          # childMessage will be a new gUpdateDataMessage
          childMessage<-new("gUpdateChildMessage", 
                            parentUpdateDataMessage=object, 
                            childName=childrenName[[i]])
          # need to know which child is being updated
          handleMessage(childMessage)
        }
      }
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
    data<-mData(object)$data
    linkData<-mData(object)$linkData
    if (is.null(linkData))
      linkData<-list()
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

    if (length(mData) > 0)
      newView<-new(type, dataName, mData)
    else
      newView<-new(type, dataName)
  }
)

##########
# 7/13/05 handle an add child message
# here ... will contain the 2 functions:
# LinkToParent and LinkToChild, which will be
# used by the gUpdateParentMessage and gUpdateChildMessage
# it will also contain the subset of the parent data that was
# used to create this new model (variable called subsetParentData)
##########
setMethod("handleMessage", "gAddChildMessage",
  function(object, ...)
  {
    # will need to follow some of the same steps as the handle message method
    # for the gAddDataMessage class
    data<-mData(object)$data
    virtualData<-mData(object)$virtualData
    name<-dataName(object)
    type<-type(object)

    loadModel(data, type, name, virtualData=virtualData)

    # then need to set some extra slots in the parent and child MVCs
    activeMVC<-get("activeMVC", mvcEnv)
    curMVC<-getMVC(activeMVC)
    childMVC<-getMVC(name)
    
    # need to get the 2 functions that will go in linkData slot
    extraPar<-list(...)
    linkToParent<-extraPar$linkToParent
    linkToChild<-extraPar$linkToChild
    subsetParentData<-extraPar$subsetParentData

    # reassign the child MVC List in the parent MVC object
    curChildList<-childMVCList(curMVC)
    curChildList[length(curChildList)+1]<-name
    childMVCList(curMVC)<-curChildList

    # assign the parentMVC in the child MVC object
    parentMVC(childMVC)<-activeMVC

    linkData(model(childMVC))<-list(linkToParent=linkToParent, 
                                    linkToChild=linkToChild)
    assign("subsetParentData", subsetParentData, controller(childMVC))

    # now need to reassign these MVC objects to the MVCList
    MVCList<-get("MVCList", mvcEnv)
    allNames <- getModelNames(sort = FALSE)
    parentIndex <- match(activeMVC, allNames)
    MVCList[[parentIndex]]<-curMVC
    childIndex <- match(name, allNames)
    MVCList[[childIndex]]<-childMVC
    assign("MVCList", MVCList, mvcEnv)
  }
)


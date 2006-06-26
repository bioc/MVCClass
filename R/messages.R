###########
# create a class for messages
###########
# a virtual class for message
setClass("gMessage")

setClass("gModifyMessage", representation(type="character", mData="list",
          dataName="character"), contains=c("gMessage", "VIRTUAL"))

setClass("gUpdateMessage", contains=c("gModifyMessage", "VIRTUAL"))

setClass("gUpdateViewMessage", contains="gUpdateMessage")

# 9/5/05 added the 'from' slot so we can loop through all linked models 
# when updating the data (without storing a global variable to ensure
# that the loop is not infinite)
setClass("gUpdateDataMessage", representation(from="character"), 
          contains="gUpdateMessage")

setClass("gAddMessage", contains=c("gModifyMessage", "VIRTUAL"))
setClass("gAddViewMessage", contains="gAddMessage")
setClass("gAddDataMessage", contains="gAddMessage")

# added 7/13/05 to create a child model
setClass("gAddChildMessage", contains="gAddMessage")

# added 7/14/05 to update a parent model
# the slot for this class tells the parent what the child update data
# message was
setClass("gSendParentMessage",
         representation(childUpdateDataMessage="gUpdateDataMessage"), 
         contains="gMessage")
# added 7/19/05 to update a child model
# the slot for this class tells the child what the parent update data
# message was
setClass("gSendChildMessage",
         representation(parentUpdateDataMessage="gUpdateDataMessage",
         childName="character"),
         contains="gMessage")

# added 6/14/06
# to ask an ancestor model for information 
# type is the type of model to look for, mData is the information being asked
# for, and from is the MVC that this message came from
setClass("gAskAncestorMessage", representation(type="character", mData="list",
           from="character"), contains="gMessage")

#####
# accessor functions
#####
if (is.null(getGeneric("from")))
  setGeneric("from", function(object)
            standardGeneric("from"))
setMethod("from", "gUpdateDataMessage", function(object)
         object@from)
setMethod("from", "gAskAncestorMessage", function(object)
         object@from)

if (is.null(getGeneric("childUpdateDataMessage")))
  setGeneric("childUpdateDataMessage", function(object)
            standardGeneric("childUpdateDataMessage"))
setMethod("childUpdateDataMessage", "gSendParentMessage", function(object)
         object@childUpdateDataMessage)

if (is.null(getGeneric("parentUpdateDataMessage")))
  setGeneric("parentUpdateDataMessage", function(object)
            standardGeneric("parentUpdateDataMessage"))
setMethod("parentUpdateDataMessage", "gSendChildMessage", function(object)
         object@parentUpdateDataMessage)

if (is.null(getGeneric("childName")))
  setGeneric("childName", function(object)
            standardGeneric("childName"))
setMethod("childName", "gSendChildMessage", function(object)
         object@childName)

if (is.null(getGeneric("type")))
  setGeneric("type", function(object)
            standardGeneric("type"))
setMethod("type", "gModifyMessage", function(object)
         object@type)
setMethod("type", "gAskAncestorMessage", function(object)
         object@type)

if (is.null(getGeneric("mData")))
  setGeneric("mData", function(object)
            standardGeneric("mData"))
setMethod("mData", "gModifyMessage", function(object)
         object@mData)
setMethod("mData", "gAskAncestorMessage", function(object)
         object@mData)

if (is.null(getGeneric("dataName")))
  setGeneric("dataName", function(object)
            standardGeneric("dataName"))
setMethod("dataName", "gModifyMessage", function(object)
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
setReplaceMethod("from", "gAskAncestorMessage", function(object, value)
         {
           object@from<-value
           object
         }
)

if (is.null(getGeneric("childUpdateDataMessage<-")))
  setGeneric("childUpdateDataMessage<-", function(object, value)
            standardGeneric("childUpdateDataMessage<-"))
setReplaceMethod("childUpdateDataMessage", "gSendParentMessage", function(object, value)
         {
           object@childUpdateDataMessage<-value
           object
         }
)

if (is.null(getGeneric("parentUpdateDataMessage<-")))
  setGeneric("parentUpdateDataMessage<-", function(object, value)
            standardGeneric("parentUpdateDataMessage<-"))
setReplaceMethod("parentUpdateDataMessage", "gSendChildMessage", function(object, value)
         {
           object@parentUpdateDataMessage<-value
           object
         }
)

if (is.null(getGeneric("childName<-")))
  setGeneric("childName<-", function(object, value)
            standardGeneric("childName<-"))
setReplaceMethod("childName", "gSendChildMessage", function(object, value)
         {
           object@childName<-value
           object
         }
)

if (is.null(getGeneric("type<-")))
  setGeneric("type<-", function(object, value)
            standardGeneric("type<-"))
setReplaceMethod("type", "gModifyMessage", function(object, value)
         {
           object@type<-value
           object
         }
)
setReplaceMethod("type", "gAskAncestorMessage", function(object, value)
         {
           object@type<-value
           object
         }
)

if (is.null(getGeneric("mData<-")))
  setGeneric("mData<-",function(object, value)
            standardGeneric("mData<-"))
setReplaceMethod("mData", "gModifyMessage", function(object, value)
         {
           object@mData<-value
           object
         }
)
setReplaceMethod("mData", "gAskAncestorMessage", function(object, value)
         {
           object@mData<-value
           object
         }
)

if (is.null(getGeneric("dataName<-")))
  setGeneric("dataName<-", function(object, value)
            standardGeneric("dataName<-"))
setReplaceMethod("dataName", "gModifyMessage", function(object, value)
         {
           object@dataName<-value
           object
         }
)


#####
# generic functions for handling messages
#####
if (is.null(getGeneric("handleMessage")))
  setGeneric("handleMessage", function(object, ...)
            standardGeneric("handleMessage"))


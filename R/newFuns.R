##########
# new functions for these new classes
##########

getMVC<-function(mName)
{
  mvcList<-get("MVCList", mvcEnv)
  allNames<-getModelNames()
  index<-match(mName, allNames)
  curMVC<-mvcList[[index]]
  return(curMVC)
}

getModel<-function(mName)
{
  curMVC<-getMVC(mName)
  curModel<-model(curMVC)
  return(curModel)
}

getActivePlot<-function()
{
  activeDev<-as.numeric(dev.cur())
  mvcList<-get("MVCList", mvcEnv)
  viewLists<-lapply(mvcList, viewList) 
  
}

getModelNames<-function()
{
  mvcList<-get("MVCList", mvcEnv)
  allNames<-unlist(lapply(lapply(mvcList, model), modelName))
  return(allNames)
}

#######
## SHOULD PUT THIS IN MVCClass package
# load the data into the mvcEnv
# data is the name of data variable
# type determines the model class
# name is the character string name of the model
# linkData is the link data
#######
loadModel<-function(data, type, name, linkData=NULL)
{
  # first create the appropriate model object
  newObject<-switch(type,
    "exprSet"=new("exprModel", data=eval(as.name(data)), name=name),
    "graph"=new("graphModel", modelData=eval(as.name(data)), modelName=name),
    "data.frame"=new("dfModel", mData=eval(as.name(data)), mName=name, 
                      linkData=linkData)
  )

  # now need to create a MVC object and add to MVCList in mvcEnv
  # leave parentMVC slot as NULL
  newMVC<-new("MVC", model=newObject, viewList=list(), childMVCList=list())

  MVCList<-get("MVCList", mvcEnv)
  if (length(MVCList) > 0)
  { 
    MVCList[length(MVCList)+1]<-newMVC
    assign("MVCList", MVCList, mvcEnv)
  }
  else
    assign("MVCList", list(newMVC), mvcEnv)
}

########
# need to get the methods that derive a new model
########
getMethodsForModel<-function(modelName)
{
  # first need to get the class type of the model
  curModel<-getModel(modelName)
  cmClass<-class(curModel)
  modelMethods<-get("modelMethods", mvcEnv)
  curMethods<-modelMethods[[cmClass]]
  return(curMethods)
}

##########
# new functions for these new classes
##########

getMVC<-function(mName)
{
  mvcList<-get("MVCList", mvcEnv)
  allNames<-getModelNames(sort=FALSE)
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

getModelNames<-function(sort=TRUE)
{
  mvcList<-get("MVCList", mvcEnv)
  if (length(mvcList) > 0)
  {
    allNames<-unlist(lapply(lapply(mvcList, model), modelName))
    # return the names in alphabetical order
    if (sort)
      return(sort(allNames))
    else
      return(allNames)
  }
  else
    return(NULL)
}

#######
## SHOULD PUT THIS IN MVCClass package
# load the data into the mvcEnv
# data is the name of data variable
# type determines the model class
# name is the character string name of the model
# linkData is the link data
#######
loadModel<-function(data, type, name, linkData=NULL, virtualData=NULL)
{
  booLoad<-TRUE
  # should check that the model name is not already in use!!!
  allMNames<-getModelNames()
  if (length(allMNames) > 0)
  {
    if (name %in% allMNames)
    {
      booLoad<-FALSE
      showLabel("That model name is already in use so a new MVC object could not be created.")
    }
  }

  if (booLoad)
  {
    # first create the appropriate model object
    newObject<-switch(type,
      "exprSet"=new("exprModel", data=data, name=name),
      "graph"=new("graphModel", modelData=data, modelName=name),
      "data.frame"=new("dfModel", mData=data, mName=name, 
                      linkData=linkData, virtualData=virtualData)
    )

    # now need to create a MVC object and add to MVCList in mvcEnv
    # leave parentMVC slot as NULL
    newMVC<-new("MVC", model=newObject, viewList=list(), parentMVC="", 
                controller=new.env(), childMVCList=list())

    # need to add the default values to the new controller environment
    setControllerDefaults(newMVC)
    createDefaultCallEvents(newMVC)
 
    MVCList<-get("MVCList", mvcEnv)
    if (length(MVCList) > 0)
    { 
      MVCList[[length(MVCList)+1]]<-newMVC
      assign("MVCList", MVCList, mvcEnv)
    }
    else
    {
      assign("MVCList", list(newMVC), mvcEnv)
      # if this is the first model, then it will be the active MVC
      assign("activeMVC", modelName(model(newMVC)), mvcEnv)
      # set the display menu
      setDisplayMenu(modelName(model(newMVC)))
    }
  }
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

############
# show a label in a Gtk window
############
showLabel<-function(lblText)
{
  win<-gtkWindow(show=FALSE)
  hbox<-gtkHBox()
  win$Add(hbox)
  lbl<-gtkLabel(lblText)
  gtkBoxPackStart(hbox,lbl,padding=5)
  okBut<-gtkButton("Ok")
  okBut$SetUsize(40,20)
  gtkBoxPackStart(hbox,okBut,padding=5)
  
  gtkAddCallback(okBut,"clicked",
    function(obj)
    {
      win$Destroy()
    }
  )
  win$Show()
}


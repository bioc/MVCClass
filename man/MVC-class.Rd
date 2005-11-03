\name{MVC-class}
\docType{class}
\alias{MVC-class}
\alias{MVC}
\alias{childMVCList<-,MVC-method}
\alias{childMVCList,MVC-method}
\alias{childMVCList}
\alias{childMVCList<-}
\alias{controller<-,MVC-method}
\alias{controller,MVC-method}
\alias{controller}
\alias{controller<-}
\alias{model<-,MVC-method}
\alias{model,MVC-method}
\alias{model}
\alias{model<-}
\alias{parentMVC<-,MVC-method}
\alias{parentMVC,MVC-method}
\alias{parentMVC}
\alias{parentMVC<-}
\alias{viewList<-,MVC-method}
\alias{viewList,MVC-method}
\alias{viewList}
\alias{viewList<-}

\title{Class "MVC": A class to represent a model-view-controller object }
\description{	 
  MVC is a class to represent a model-view-controller object.  The MVC class
  will combine these three components (model, view and controller) so that
  the MVC objects can be reused and linked.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MVC", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{model}:}{the model object (will inherit from the gModel
    virtual class) }
    \item{\code{viewList}:}{a list of the view objects that visualize the
    model }
    \item{\code{controller}:}{the environment that stores information for the
    MVC object }
    \item{\code{parentMVC}:}{the name of the parent model (MVC) if there is a
    parent model }
    \item{\code{childMVCList}:}{a list of the names of the child models
    (MVCs); it may be an empty list if there are no children}
  }
}
\section{Methods}{
  \describe{
    \item{childMVCList<-}{Sets the \code{childMVCList} slot }
    \item{childMVCList}{Returns the \code{childMVCList} slot }
    \item{controller<-}{Sets the \code{controller} slot }
    \item{controller}{Returns the \code{controller} slot }
    \item{model<-}{Sets the \code{model} slot }
    \item{model}{Returns the \code{model} slot }
    \item{parentMVC<-}{Sets the \code{parentMVC} slot }
    \item{parentMVC}{Returns the \code{parentMVC} slot }
    \item{viewList<-}{Sets the \code{viewList} slot }
    \item{viewList}{Returns the \code{viewList} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gModel-class}},
  \code{\link{genView-class}}
}
\examples{
}
\keyword{classes}

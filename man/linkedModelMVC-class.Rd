\name{linkedModelMVC-class}
\docType{class}
\alias{linkedModelMVC-class}
\alias{linkedModelMVC}
\alias{parentMVC<-,linkedModelMVC-method}
\alias{parentMVC,linkedModelMVC-method}
\alias{parentMVC}
\alias{parentMVC<-}
\alias{childMVCList<-,linkedModelMVC-method}
\alias{childMVCList,linkedModelMVC-method}
\alias{childMVCList}
\alias{childMVCList<-}

\title{Class "linkedModelMVC": A class to represent a MVC object that can be
linked to other MVC objects }
\description{	
  linkedModelMVC is a class to represent a model-view-controller object that
  can be linked to other MVC objects.  The linkedModelMVC class
  will combine these three components (model, view and controller) so that
  the MVC objects can be reused and it has the slots, parentMVC and
  childMVCList, to link this MVC object with other MVC objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("linkedModelMVC", ...)}. 
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
\section{Extends}{
Class \code{"singleModelMVC"}, directly.
Class \code{"MVC"}, by class \code{"singleModelMVC"}.
}
\section{Methods}{
  \describe{
    \item{childMVCList<-}{Sets the \code{childMVCList} slot }
    \item{childMVCList}{Returns the \code{childMVCList} slot }
    \item{parentMVC<-}{Sets the \code{parentMVC} slot }
    \item{parentMVC}{Returns the \code{parentMVC} slot }	 
  }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gModel-class}},
  \code{\link{genView-class}},
  \code{\link{singleModelMVC-class}},
  \code{\link{MVC-class}}
}
\examples{
}
\keyword{classes}

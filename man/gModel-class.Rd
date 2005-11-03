\name{gModel-class}
\docType{class}
\alias{gModel-class}
\alias{gModel}
\alias{linkData<-,gModel-method}
\alias{linkData,gModel-method}
\alias{linkData}
\alias{linkData<-}
\alias{modelData<-,gModel-method}
\alias{modelData,gModel-method}
\alias{modelData}
\alias{modelData<-}
\alias{modelName<-,gModel-method}
\alias{modelName,gModel-method}
\alias{modelName}
\alias{modelName<-}
\alias{virtualData<-,gModel-method}
\alias{virtualData,gModel-method}
\alias{virtualData}
\alias{virtualData<-}
\alias{updateModel}

\title{Class "gModel": A virtual class for models }
\description{
  gModel is a virtual class from which all other model classes will inherit.
  Model objects will be responsible for storing and updating the data sets.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{modelData}:}{the actual model data }
    \item{\code{linkData}:}{a list of the functions to link this data to its
    parent and child models (if it has any) }
    \item{\code{virtualData}:}{data that pertains to the views of this model }
    \item{\code{modelName}:}{the name of the model (will be the same name as
    the MVC) }
  }
}
\section{Methods}{
  \describe{
    \item{linkData<-}{Sets the \code{linkData} slot }
    \item{linkData}{Returns the \code{linkData} slot }
    \item{modelData<-}{Sets the \code{modelData} slot }
    \item{modelData}{Returns the \code{modelData} slot }
    \item{modelName<-}{Sets the \code{modelName} slot }
    \item{modelName}{Returns the \code{modelName} slot }
    \item{virtualData<-}{Sets the \code{virtualData} slot }
    \item{virtualData}{Returns the \code{virtualData} slot }
	 }
Also, all models will have an updateModel method that will be defined in the
packages that use this package (for example, iSPlot and iSNetwork).  The
updateModel method will be called by a gUpdateDataMessage object when the data
needs to be updated.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{dfModel-class}},
  \code{\link{exprModel-class}},
  \code{\link{graphModel-class}}
}
\examples{
}
\keyword{classes}

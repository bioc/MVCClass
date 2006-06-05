\name{dfModel-class}
\docType{class}
\alias{dfModel-class}
\alias{dfModel}
\title{Class "dfModel": A class to represent a data frame model }
\description{	 
  dfModel is a class to represent a data frame model.  This class inherits
  from the virtual class, gModel.  An object of dfModel is responsible for
  storing and updating the data.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("dfModel", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork and iSPlot packages). 
}
\section{Slots}{
	 \describe{
    \item{\code{modelData}:}{the model data, which is a data frame }
    \item{\code{virtualData}:}{data that is needed by views of this model,
    will be a data frame }
    \item{\code{linkData}:}{a list of functions that link this model to its
    parent and child models (if it has any) }
    \item{\code{modelName}:}{the name of this model }
  }
}
\section{Extends}{
Class \code{"gModel"}, directly.
}
\section{Methods}{
No methods defined with class "dfModel" in the signature.  The methods for
this class will be created in other packages that use this package like
iSNetwork and iSPlot.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gModel-class}}
}
\examples{
}
\keyword{classes}

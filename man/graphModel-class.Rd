\name{graphModel-class}
\docType{class}
\alias{graphModel-class}
\alias{graphModel}
\title{Class "graphModel": A class to represent a graph model }
\description{	 
  graphModel is a class to represent a graph model.  This class inherits
  from the virtual class, gModel.  An object of graphModel is responsible for
  storing and updating the data.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("graphModel", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{modelData}:}{the model data, which is a graph object }
    \item{\code{linkData}:}{a list of functions that link this model to its
    parent and child models (if it has any) }
    \item{\code{virtualData}:}{data that is needed by views of this model }
    \item{\code{modelName}:}{the name of this model }
  }
}
\section{Extends}{
Class \code{"gModel"}, directly.
}
\section{Methods}{
No methods defined with class "graphModel" in the signature.  The methods for
this class will be created in other packages that use this package like
iSNetwork.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gModel-class}},
  \code{\link{exprModel-class}},
  \code{\link{dfModel-class}}
}
\examples{
}
\keyword{classes}

\name{gUpdateViewMessage-class}
\docType{class}
\alias{gUpdateViewMessage-class}
\alias{gUpdateViewMessage}

\title{Class "gUpdateViewMessage": A class to represent an update view message }
\description{	 
  gUpdateViewMessage is a class to represent an update view message.
  gUpdateViewMessage is inherited from the virtual class, gUpdateMessage.
  Whenever a gUpdateViewMessage
  is created (initialized), the next step is to call the handleMessage method 
  to act upon that message and update the views.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gUpdateViewMessage",
...)}. 
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork and iSPlot packages).
}
\section{Slots}{
	 \describe{
    \item{\code{type}:}{the type of update to be performed }
    \item{\code{mData}:}{a list of the information needed to perform the update }
    \item{\code{dataName}:}{the name of the data set that was updated (views
    are updated after the data set has been updated) }
  }
}
\section{Extends}{
Class \code{"gUpdateMessage"}, directly.
Class \code{"gModifyMessage"}, by class \code{"gUpdateMessage"}.
Class \code{"gMessage"}, by class \code{"gUpdateMessage"}.
}
\section{Methods}{
No methods defined with class "gUpdateViewMessage" in the signature.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gUpdateDataMessage-class}},
  \code{\link{gUpdateMessage-class}},
  \code{\link{gMessage-class}}	
}
\examples{
}
\keyword{classes}

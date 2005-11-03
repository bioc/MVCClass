\name{gUpdateDataMessage-class}
\docType{class}
\alias{gUpdateDataMessage-class}
\alias{gUpdateDataMessage}
\alias{from<-,gUpdateDataMessage-method}
\alias{from,gUpdateDataMessage-method}
\alias{from}
\alias{from<-}

\title{Class "gUpdateDataMessage": A class to represent an update data message }
\description{
  gUpdateDataMessage is a class to represent an update data message.
  gUpdateDataMessage is inherited from the class, gUpdateMessage, which
  inherits from the virtual class, gMessage.  Whenever a gUpdateDataMessage
  is created (initialized), the next step is to call the handleMessage method
  to act upon that message and update the data.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gUpdateDataMessage",
...)}. 
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork and iSPlot packages).  
}
\section{Slots}{
	 \describe{
    \item{\code{from}:}{the name of the MVC (model) that this update message
    came from }
    \item{\code{type}:}{the type of update to perform }
    \item{\code{mData}:}{a list of the information needed to perform the update }
    \item{\code{dataName}:}{the name of the data set to be updated }
  }
}
\section{Extends}{
Class \code{"gUpdateMessage"}, directly.
Class \code{"gMessage"}, by class \code{"gUpdateMessage"}.
}
\section{Methods}{
  \describe{
    \item{from<-}{Sets the \code{from} slot }
    \item{from}{Returns the \code{from} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gUpdateViewMessage-class}},
  \code{\link{gUpdateMessage-class}},
  \code{\link{gMessage-class}}
}
\examples{
}
\keyword{classes}

\name{gSendParentMessage-class}
\docType{class}
\alias{gSendParentMessage-class}
\alias{gSendParentMessage}
\alias{childUpdateDataMessage<-,gSendParentMessage-method}
\alias{childUpdateDataMessage,gSendParentMessage-method}
\alias{childUpdateDataMessage}
\alias{childUpdateDataMessage<-}

\title{Class "gSendParentMessage": A class to represent a send parent MVC
message}
\description{	 
  gSendParentMessage is a class to represent a send parent MVC message.
  Whenever a model is updated and that model has a parent MVC, then an object
  of this class will be created to notify the parent MVC that a child model
  was updated.  gUpdateParentMessage inherits from the virtual class,
  gMessage.  The initialize and handleMessage methods
  will be defined in other packages that use this class (for example, the
  iSNetwork package).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gSendParentMessage",
...)}. 
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{childUpdateDataMessage}:}{an object of class
    gUpdateDataMessage that was used to update the child model (MVC) }
  }
}
\section{Extends}{
Class \code{"gMessage"}, directly.
}
\section{Methods}{
  \describe{
    \item{childUpdateDataMessage<-}{Sets the \code{childUpdateDataMessage}
    slot }
    \item{childUpdateDataMessage}{Returns the \code{childUpdateDataMessage} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gSendChildMessage-class}},
  \code{\link{gMessage-class}}
}
\examples{
}
\keyword{classes}

\name{gSendChildMessage-class}
\docType{class}
\alias{gSendChildMessage-class}
\alias{gSendChildMessage}
\alias{childName<-,gSendChildMessage-method}
\alias{childName,gSendChildMessage-method}
\alias{childName}
\alias{childName<-}
\alias{parentUpdateDataMessage<-,gSendChildMessage-method}
\alias{parentUpdateDataMessage,gSendChildMessage-method}
\alias{parentUpdateDataMessage}
\alias{parentUpdateDataMessage<-}

\title{Class "gSendChildMessage": A class to represent a send child MVC
message }
\description{	 
  gSendChildMessage is a class to represent a send child MVC message.
  Whenever a model is updated and that model has a child MVC, then an object
  of this class will be created to notify the child MVC that a parent model
  was updated.  gSendChildMessage inherits from the virtual class,
  gMessage.  The initialize and handleMessage methods
  will be defined in other packages that use this class (for example, the
  iSNetwork package). 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gSendChildMessage",
...)}. 
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{parentUpdateDataMessage}:}{an object of class
    gUpdateDataMessage that was used to update the parent model (MVC) }
    \item{\code{childName}:}{the name of the child model that this message is
    directed towards (because a parent MVC can have multiple child MVCs) }
  }
}
\section{Extends}{
Class \code{"gMessage"}, directly.
}
\section{Methods}{
  \describe{
    \item{childName<-}{Sets the \code{childName} slot }
    \item{childName}{Returns the \code{childName} slot }
    \item{parentUpdateDataMessage<-}{Sets the \code{parentUpdateDataMessage}
    slot }
    \item{parentUpdateDataMessage}{Returns the \code{parentUpdateDataMessage}
    slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gSendParentMessage-class}},
  \code{\link{gMessage-class}}
}
\examples{
}
\keyword{classes}

\name{gUpdateChildMessage-class}
\docType{class}
\alias{gUpdateChildMessage-class}
\alias{gUpdateChildMessage}
\alias{childName<-,gUpdateChildMessage-method}
\alias{childName,gUpdateChildMessage-method}
\alias{childName}
\alias{childName<-}
\alias{parentUpdateDataMessage<-,gUpdateChildMessage-method}
\alias{parentUpdateDataMessage,gUpdateChildMessage-method}
\alias{parentUpdateDataMessage}
\alias{parentUpdateDataMessage<-}

\title{Class "gUpdateChildMessage": A class to represent an update child MVC
message }
\description{	 
  gUpdateChildMessage is a class to represent an update child MVC message.
  Whenever a model is updated if that model has a child MVC, then an object
  of this class will be created to notify the child MVC that a parent model
  was updated.  gUpdateChildMessage inherits from the virtual class,
  gMessage. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gUpdateChildMessage",
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
  \code{\link{gUpdateParentMessage-class}},
  \code{\link{gMessage-class}}
}
\examples{
}
\keyword{classes}

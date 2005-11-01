\name{gUpdateParentMessage-class}
\docType{class}
\alias{gUpdateParentMessage-class}
\alias{gUpdateParentMessage}
\alias{childUpdateDataMessage<-,gUpdateParentMessage-method}
\alias{childUpdateDataMessage,gUpdateParentMessage-method}
\alias{childUpdateDataMessage}
\alias{childUpdateDataMessage<-}

\title{Class "gUpdateParentMessage": A class to represent an update parent MVC
message}
\description{	 
  gUpdateParentMessage is a class to represent an update parent MVC message.
  Whenever a model is updated if that model has a parent MVC, then an object
  of this class will be created to notify the parent MVC that a child model
  was updated.  gUpdateParentMessage inherits from the virtual class,
  gMessage. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gUpdateParentMessage",
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
    \item{childUpdateDataMessage<-}{Returns the \code{childUpdateDataMessage}
    slot }
    \item{childUpdateDataMessage}{Sets the \code{childUpdateDataMessage} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gUpdateChildMessage-class}},
  \code{\link{gMessage-class}}
}
\examples{
}
\keyword{classes}

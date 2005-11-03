\name{gUpdateMessage-class}
\docType{class}
\alias{gUpdateMessage-class}
\alias{gUpdateMessage}
\alias{dataName<-,gUpdateMessage-method}
\alias{dataName,gUpdateMessage-method}
\alias{mData<-,gUpdateMessage-method}
\alias{mData,gUpdateMessage-method}
\alias{type<-,gUpdateMessage-method}
\alias{type,gUpdateMessage-method}

\title{Class "gUpdateMessage": A class to represent an update message}
\description{	 
  gUpdateMessage is a class to represent an update message.  Both
  gUpdateViewMessage and gUpdateDataMessage classes are inherited from
  gUpdateMessage.   Although, gUpdateMessage is not a virtual class, it is not
  intended to have objects of gUpdateMessage class created.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gUpdateMessage", ...)}.
However, remember that objects of this class should not be created.
}
\section{Slots}{
	 \describe{
    \item{\code{type}:}{the type of update to be performed, will be a
    character string }
    \item{\code{mData}:}{a list of information needed to perform the update }
    \item{\code{dataName}:}{the name of the data set to be updated }
  }
}
\section{Extends}{
Class \code{"gMessage"}, directly.
}
\section{Methods}{
  \describe{
    \item{dataName<-}{Sets the \code{dataName} slot }
    \item{dataName}{Returns the \code{dataName} slot }
    \item{mData<-}{Sets the \code{mData} slot }
    \item{mData}{Returns the \code{mData} slot }
    \item{type<-}{Sets the \code{type} slot }
    \item{type}{Returns the \code{type} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gMessage-class}},
  \code{\link{gUpdateViewMessage-class}},
  \code{\link{gUpdateDataMessage-class}} 
}
\examples{
}
\keyword{classes}

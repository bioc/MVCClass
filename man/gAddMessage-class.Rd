\name{gAddMessage-class}
\docType{class}
\alias{gAddMessage-class}
\alias{gAddMessage}
\alias{dataName<-,gAddMessage-method}
\alias{dataName,gAddMessage-method}
\alias{dataName}
\alias{dataName<-}
\alias{mData<-,gAddMessage-method}
\alias{mData,gAddMessage-method}
\alias{mData}
\alias{mData<-}
\alias{type<-,gAddMessage-method}
\alias{type,gAddMessage-method}
\alias{type}
\alias{type<-}

\title{Class "gAddMessage":  A class to represent an add message}
\description{	 
  gAddMessage is a class to represent an add message.  Both gAddDataMessage
  and gAddViewMessage classes are inherited from gAddMessage.  Although,
  gAddMessage is not a virtual class, it is not intended to have objects of
  gAddMessage class created.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gAddMessage", ...)}.
However, remember that objects of this class should not be created.
}
\section{Slots}{
	 \describe{
    \item{\code{dataName}:}{the name of the data to be added or the name of
    the data that the view will visualize }
    \item{\code{mData}:}{a list of information needed to perform the addition }
    \item{\code{type}:}{the type of addition to perform (ex. which type of
    model to add or which type of view to add) }
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
  \code{\link{gAddViewMessage-class}},
  \code{\link{gAddDataMessage-class}},
  \code{\link{gAddChildMessage-class}}
}
\examples{
}
\keyword{classes}

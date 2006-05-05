\name{gModifyMessage-class}
\docType{class}
\alias{gModifyMessage-class}
\alias{gModifyMessage}
\alias{dataName<-,gModifyMessage-method}
\alias{dataName,gModifyMessage-method}
\alias{dataName<-}
\alias{dataName}
\alias{mData<-,gModifyMessage-method}
\alias{mData,gModifyMessage-method}
\alias{mData<-}
\alias{mData}
\alias{type<-,gModifyMessage-method}
\alias{type,gModifyMessage-method}
\alias{type<-}
\alias{type}

\title{Class "gModifyMessage": A class to represent a modify message }
\description{	 
  gModifyMessage is a virtual class to represent a modify message.  Both
  gAddMessage and gUpdateMessage classes are inherited from gModifyMessage.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
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
  \code{\link{gAddMessage-class}},
  \code{\link{gUpdateMessage-class}}
}
\examples{
}
\keyword{classes}

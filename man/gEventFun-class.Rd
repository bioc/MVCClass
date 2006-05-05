\name{gEventFun-class}
\docType{class}
\alias{gEventFun-class}
\alias{gEventFun}
\alias{callFun<-,gEventFun-method}
\alias{callFun,gEventFun-method}
\alias{callFun}
\alias{callFun<-}
\alias{preprocessFun<-,gEventFun-method}
\alias{preprocessFun,gEventFun-method}
\alias{preprocessFun}
\alias{preprocessFun<-}
\alias{shortName<-,gEventFun-method}
\alias{shortName,gEventFun-method}
\alias{shortName}
\alias{shortName<-}

\title{A class to link a callback function with an event}
\description{	 
  The gEventFun class creates an object which contains all the needed
  information to link a callback function to an event.  The gEventFun object
  will hold all of the callback function information, including the callback
  function, a short description of what the callback function does, and a
  list of all preprocessing functions that must be called.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gEventFun", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{callFun}:}{the callback function}
    \item{\code{shortName}:}{a short description of what the callback function
    does}
    \item{\code{preprocessFun}:}{a list of preprocessing functions that must
    be called before the callback function (this can be NULL)}
  }
}
\section{Methods}{
  \describe{
    \item{callFun<-}{Sets the \code{callFun} slot }
    \item{callFun}{Returns the \code{callFun} slot }
    \item{preprocessFun<-}{Sets the \code{preprocessFun} slot }
    \item{preprocessFun}{Returns the \code{preprocessFun} slot }
    \item{shortName<-}{Sets the \code{shortName} slot }
    \item{shortName}{Returns the \code{preprocessFun} slot }
	 }
}
\author{ Elizabeth Whalen }
\examples{
}
\keyword{classes}

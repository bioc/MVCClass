\name{gAskAncestorMessage-class}
\docType{class}
\alias{gAskAncestorMessage-class}
\alias{from<-,gAskAncestorMessage-method}
\alias{from,gAskAncestorMessage-method}
\alias{mData<-,gAskAncestorMessage-method}
\alias{mData,gAskAncestorMessage-method}
\alias{type<-,gAskAncestorMessage-method}
\alias{type,gAskAncestorMessage-method}

\title{Class "gAskAncestorMessage": A class to represent a question asking for
data from an ancestor model }
\description{ 
  gAskAncestorMessage is a class to ask for model data from an ancestor model
  (MVC).  For example, if a user wants to create a child model, but needs data
  from both its parent and its grandparent model.  Then when the parent model
  creates the new child model it would ask its parent (the grandparent of the
  new child model) for data using this class.  

  A specific example is if the original model contains microarray expression
  data and a child model of this model is the GO graph.  Then if the user
  wants to look at the expression data of only genes annotated at a particular
  node, a child model can be created from the GO graph, but this new child
  model also needs data from the original model (its grandparent).  Thus, when
  the GO graph creates a child model it needs to ask its parent for the
  expression data. 
}
\section{Objects from the Class}{
  Objects can be created by calls of the form 
      \code{new("gAskAncestorMessage", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{type}:}{which model type to look for as an ancestor (this is a
  character string naming the class)}
    \item{\code{mData}:}{the data that is being requested }
    \item{\code{from}:}{the MVC that asked the question }
  }
}
\section{Extends}{
Class \code{"gMessage"}, directly.
}
\section{Methods}{
  \describe{
    \item{from<-}{Sets the \code{from} slot }
    \item{from}{Returns the \code{from} slot }
    \item{mData<-}{Sets the \code{mData} slot }
    \item{mData}{Returns the \code{mData} slot }
    \item{type<-}{Sets the \code{type} slot }
    \item{type}{Returns the \code{type} slot }
	 }
}
\author{ Elizabeth Whalen }
\note{ 
  Even though this class is used to ask for information from an ancestor, each
  model (MVC) has only parent.  This is important when considering how data
  sets are linked.
}
\seealso{
  \code{\link{gMessage-class}} 
}
\examples{
}
\keyword{classes}


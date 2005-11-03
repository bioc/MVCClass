\name{spreadView-class}
\docType{class}
\alias{spreadView-class}
\alias{spreadView}
\alias{clist<-,spreadView-method}
\alias{clist,spreadView-method}
\alias{clist}
\alias{clist<-}

\title{Class "spreadView": A class to represent a spreadsheet view }
\description{	 
  spreadView is a class to represent a view that is a spreadsheet. spreadView
  inherits from the virtual class, genView.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("spreadView", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork and iSPlot packages). 
}
\section{Slots}{
	 \describe{
    \item{\code{clist}:}{an object of class \code{"GtkCList"}, i.e. the
    spreadsheet }
    \item{\code{dataName}:}{a character string describing what data is shown
    in the view }
    \item{\code{win}:}{an object of class \code{"GtkWindow"} that holds the
    view }
    \item{\code{winNum}:}{a number that tells what number view this is (for
    example, the first view created will have winNum=1) }
  }
}
\section{Extends}{
Class \code{"genView"}, directly.
}
\section{Methods}{
  \describe{
    \item{clist<-}{Sets the \code{clist} slot }
    \item{clist}{Returns the \code{clist} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{genView-class}},
  \code{\link{plotView-class}}
}
\examples{
}
\keyword{classes}

\name{graphView-class}
\docType{class}
\alias{graphView-class}
\alias{graphView}
\title{Class "graphView": A class to represent a graph view }
\description{	 
  graphView is a class to represent a view that is a graph.  graphView
  inherits from the class, plotView, which inherits from the virtual class,
  genView. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("graphView", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{plotDevice}:}{the plot device number }
    \item{\code{plotPar}:}{the parameter list for the plot, see par() }
    \item{\code{drArea}:}{an object of class \code{"GtkDrawingArea"} }
    \item{\code{dataName}:}{a character string describing what data is shown
    in the view }
    \item{\code{win}:}{an object of class \code{"GtkWindow"} that holds the
    view }
    \item{\code{winNum}:}{a number that tells what number view this is (for
    example, the first view created will have winNum=1) }
  }
}
\section{Extends}{
Class \code{"plotView"}, directly.
Class \code{"genView"}, by class \code{"plotView"}.
}
\section{Methods}{
No methods defined with class "graphView" in the signature.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{genView-class}},
  \code{\link{plotView-class}}
}
\examples{
}
\keyword{classes}

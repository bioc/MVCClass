\name{plotView-class}
\docType{class}
\alias{plotView}
\alias{plotView-class}
\alias{drArea<-,plotView-method}
\alias{drArea,plotView-method}
\alias{drArea}
\alias{drArea<-}
\alias{plotDevice<-,plotView-method}
\alias{plotDevice,plotView-method}
\alias{plotDevice}
\alias{plotDevice<-}
\alias{plotPar<-,plotView-method}
\alias{plotPar,plotView-method}
\alias{plotPar}
\alias{plotPar<-}
\alias{clickEvent}
\alias{motionEvent}

\title{Class "plotView": A class to represent a plot view }
\description{	 
  plotView is a class to represent a view that is a plot.  Any particular
  types of plots can inherit from this class.  For instance, sPlotView, is
  a class that inherits from plotView and represents a scatterplot view.
  Although plotView is not a virtual class, it is not intended to create
  objects of class, plotType.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("plotView", ...)}.
Objects of this class should not be created.
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
Class \code{"genView"}, directly.
}
\section{Methods}{
  \describe{
    \item{drArea<-}{Returns the \code{drArea} slot }
    \item{drArea}{Sets the \code{drArea} slot }
    \item{plotDevice<-}{Returns the \code{plotDevice} slot }
    \item{plotDevice}{Sets the \code{plotDevice} slot }
    \item{plotPar<-}{Returns the \code{plotPar} slot }
    \item{plotPar}{Sets the \code{plotPar} slot }
	 }
Also, all classes that inherit from plotView will have clickEvent and
motionEvent methods.  The clickEvent method will be called whenever a user
clicks on the plot and the motionEvent method will be called whenever a user
moves the cursor over the plot.  These methods will be defined in packages
that use this package, such as iSPlot and iSNetwork.  Note that the clickEvent
method will also be defined for the spreadView class and this will correspond
to a user selecting a row on the spreadsheet.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{genView-class}},
  \code{\link{sPlotView-class}},
  \code{\link{graphView-class}},
  \code{\link{heatmapView-class}}
}
\examples{
}
\keyword{classes}

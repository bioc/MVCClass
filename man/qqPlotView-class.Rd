\name{qqPlotView-class}
\docType{class}
\alias{qqPlotView-class}
\alias{qqPlotView}
\alias{xval<-,qqPlotView-method}
\alias{xval,qqPlotView-method}
\alias{xval<-}
\alias{xval}
\alias{yval<-,qqPlotView-method}
\alias{yval,qqPlotView-method}
\alias{yval<-}
\alias{yval}
\title{Class "qqPlotView": A class to represent a qq-plot }
\description{
  qqPlotView is a class to represent a qq-plot.  For now, this class will be
  used to create qq-plot views of gene set enrichment data (stored in the
  gseModel).  qqPlotView inherits from the class, plotView, which inherits
  from the virtual class, genView. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("qqPlotView", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package).
}
\section{Slots}{
	 \describe{
    \item{\code{xval}:}{the x values for the points plotted }
    \item{\code{yval}:}{the y values for the points plotted }
    \item{\code{plotDevice}:}{the plot device number }
    \item{\code{plotPar}:}{the parameter list for the plot, see par() }
    \item{\code{drArea}:}{an object of class \code{"GtkDrawingArea"} }
    \item{\code{dataName}:}{ a character string describing what data are shown
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
  \describe{
    \item{xval<-}{Sets the \code{xval} slot}
    \item{xval}{Returns the \code{xval} slot }
    \item{yval<-}{Sets the \code{yval} slot }
    \item{yval}{Returns the \code{yval} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{genView-class}},
  \code{\link{plotView-class}},
  \code{\link{sPlotView-class}}
}
\examples{
}
\keyword{classes}

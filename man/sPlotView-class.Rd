\name{sPlotView-class}
\docType{class}
\alias{sPlotView-class}
\alias{sPlotView}
\alias{colx<-,sPlotView-method}
\alias{colx,sPlotView-method}
\alias{colx}
\alias{colx<-}
\alias{coly<-,sPlotView-method}
\alias{coly,sPlotView-method}
\alias{coly}
\alias{coly<-}
\alias{dfRows<-,sPlotView-method}
\alias{dfRows,sPlotView-method}
\alias{dfRows}
\alias{dfRows<-}

\title{Class "sPlotView": A class to represent a scatterplot view }
\description{	 
  sPlotView is a class to represent a view that is a scatterplot.  
  sPlotView inherits from the class, plotView, which inherits from the
  virtual class, genView.   
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sPlotView", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork and iSPlot packages).
}
\section{Slots}{
	 \describe{
    \item{\code{dfRows}:}{the names of the data frame rows that will be
    plotted }
    \item{\code{colx}:}{the name of data frame column that will be plotted on
    the x axis }
    \item{\code{coly}:}{the name of the data frame column that will be plotted
    on the y axis}
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
  \describe{
    \item{colx<-}{Sets the \code{colx} slot }
    \item{colx}{Returns the \code{colx} slot }
    \item{coly<-}{Sets the \code{coly} slot }
    \item{coly}{Returns the \code{coly} slot }
    \item{dfRows<-}{Sets the \code{dfRows} slot }
    \item{dfRows}{Returns the \code{dfRows} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{genView-class}},
  \code{\link{plotView-class}},
  \code{\link{spreadView-class}}
}
\examples{
}
\keyword{classes}

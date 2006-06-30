\name{genView-class}
\docType{class}
\alias{genView-class}
\alias{genView}
\alias{dataName<-,genView-method}
\alias{dataName,genView-method}
\alias{win<-,genView-method}
\alias{win,genView-method}
\alias{win}
\alias{win<-}
\alias{winNum<-,genView-method}
\alias{winNum,genView-method}
\alias{winNum}
\alias{winNum<-}
\alias{updateView}
\alias{redrawView}
\alias{identifyView}

\title{Class "genView": A virtual class to describe a view  }
\description{	 
  genView is a virtual class that all view classes inherit from.  All views
  will contain the information of what window (slot win) they are stored in, 
  what data (slot dataName) is shown in the view, and the number of the 
  window (slot winNum) that shows the view.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{dataName}:}{a character string describing what data are shown
    in the view }
    \item{\code{win}:}{an object of class \code{"GtkWindow"} that holds the
    view }
    \item{\code{winNum}:}{a number that tells what number view this is (for
    example, the first view created will have winNum=1) }
  }
}
\section{Methods}{
  \describe{
    \item{dataName<-}{Sets the \code{dataName} slot }
    \item{dataName}{Returns the \code{dataName} slot }
    \item{win<-}{Sets the \code{win} slot }
    \item{win}{Returns the \code{win} slot }
    \item{winNum<-}{Sets the \code{winNum} slot }
    \item{winNum}{Returns the \code{winNum} slot }
	 }
Also, all view objects will have two methods, redrawView and updateView, that
will define how a view should be redrawn when the underlying data has
changed.  The redrawView method will completely redraw the view, while the
updateView method will only redraw the parts of the view that have changed.
Both of these methods, redrawView and updateView, will be defined in packages
that use this package, such as iSPlot and iSNetwork.  Also, all views will
have an identifyView method that will identify an object (such as a point, a
node, a row, etc.) on the view given some location on the view (such as the
user coordinates).   
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{plotView-class}},
  \code{\link{sPlotView-class}},
  \code{\link{spreadView-class}}
}
\examples{
}
\keyword{classes}

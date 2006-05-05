\name{gAddViewMessage-class}
\docType{class}
\alias{gAddViewMessage-class}
\alias{gAddViewMessage}
\title{Class "gAddViewMessage": A class to represent an add view message }
\description{	
  gAddViewMessage is a class to represent an add view message.  Whenever
  a view needs to be added, a gAddViewMessage object is created and the
  handleMessage method is called to act on the message.  gAddViewMessage
  inherits from the virtual class, gAddMessage. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gAddViewMessage",
...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork and iSPlot packages). 
}
\section{Slots}{
	 \describe{
    \item{\code{dataName}:}{the name of the model to be added }
    \item{\code{mData}:}{a list of information that contains the data needed
    to add the view }
    \item{\code{type}:}{the type of view to be added (ex. scatterplot,
    spreadsheet, etc.) }
  }
}
\section{Extends}{
Class \code{"gAddMessage"}, directly.
Class \code{"gModifyMessage"}, by class \code{"gAddMessage"}.
Class \code{"gMessage"}, by class \code{"gAddMessage"}.
}
\section{Methods}{
No methods defined with class "gAddViewMessage" in the signature.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gAddDataMessage-class}},
  \code{\link{gAddMessage-class}},
  \code{\link{gMessage-class}}	
}
\examples{
}
\keyword{classes}

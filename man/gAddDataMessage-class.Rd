\name{gAddDataMessage-class}
\docType{class}
\alias{gAddDataMessage-class}
\alias{gAddDataMessage}

\title{Class "gAddDataMessage": A class to represent an add data message }
\description{	
  gAddDataMessage is a class to represent an add data message.  Whenever
  a model needs to be added, a gAddDataMessage object
  is created and the handleMessage method is called to act on the message.
  gAddDataMessage inherits from the virtual class, gAddMessage.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gAddDataMessage",
...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork and iSPlot packages). 
}
\section{Slots}{
	 \describe{
    \item{\code{dataName}:}{the name of the model to be added }
    \item{\code{mData}:}{a list of information that contains the data to be added }
    \item{\code{type}:}{the type of model to be added (for now, it may be a
    data.frame, ExpressionSet, or graph) }
  }
}
\section{Extends}{
Class \code{"gAddMessage"}, directly.
Class \code{"gModifyMessage"}, by class \code{"gAddMessage"}.
Class \code{"gMessage"}, by class \code{"gAddMessage"}.
}
\section{Methods}{
No methods defined with class "gAddDataMessage" in the signature.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gAddViewMessage-class}},
  \code{\link{gAddMessage-class}},
  \code{\link{gMessage-class}}
}
\examples{
}
\keyword{classes}

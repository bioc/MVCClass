\name{gAddChildMessage-class}
\docType{class}
\alias{gAddChildMessage-class}
\alias{gAddChildMessage}
\title{Class "gAddChildMessage": A class to represent an add child MVC message }
\description{	 
  gAddChildMessage class represents an add child MVC message.  This class will
  create a new MVC object that is a child of the current MVC object.
  gAddChildMessage inherits from the class, gAddMessage, which inherits from
  the virtual class, gMessage.  The initialize and handleMessage methods will
  be defined in other packages that use this class (for example, the iSNetwork
  package). 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gAddChildMessage",
...)}. 
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{dataName}:}{the name of the new child model (MVC) }
    \item{\code{mData}:}{a list of information needed to add the new model }
    \item{\code{type}:}{the type of the new model (for now, it may be a
    data.frame, exprSet, or graph) }
  }
}
\section{Extends}{
Class \code{"gAddMessage"}, directly.
Class \code{"gMessage"}, by class \code{"gAddMessage"}.
}
\section{Methods}{
No methods defined with class "gAddChildMessage" in the signature.
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

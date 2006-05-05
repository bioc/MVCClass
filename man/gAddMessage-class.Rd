\name{gAddMessage-class}
\docType{class}
\alias{gAddMessage-class}
\alias{gAddMessage}

\title{Class "gAddMessage":  A class to represent an add message}
\description{	 
  gAddMessage is a virtual class to represent an add message.  Both
  gAddDataMessage and gAddViewMessage classes are inherited from gAddMessage.
}
\section{Objects from the Class}{A virtual Class: No objects may be created
from it. }
\section{Slots}{
	 \describe{
    \item{\code{dataName}:}{the name of the data to be added or the name of
    the data that the view will visualize }
    \item{\code{mData}:}{a list of information needed to perform the addition }
    \item{\code{type}:}{the type of addition to perform (ex. which type of
    model to add or which type of view to add) }
  }
}
\section{Extends}{
Class \code{"gModifyMessage"}, directly.
Class \code{"gMessage"}, by class \code{"gModifyMessage"}.
}
\section{Methods}{
No methods defined with class "gAddMessage" in the signature.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gMessage-class}},
  \code{\link{gModifyMessage-class}},
  \code{\link{gAddViewMessage-class}},
  \code{\link{gAddDataMessage-class}},
  \code{\link{gAddChildMessage-class}}
}
\examples{
}
\keyword{classes}

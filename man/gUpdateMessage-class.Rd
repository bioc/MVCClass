\name{gUpdateMessage-class}
\docType{class}
\alias{gUpdateMessage-class}
\alias{gUpdateMessage}

\title{Class "gUpdateMessage": A class to represent an update message}
\description{	 
  gUpdateMessage is a virtual class to represent an update message.  Both
  gUpdateViewMessage and gUpdateDataMessage classes are inherited from
  gUpdateMessage.   
}
\section{Objects from the Class}{A virtual Class: No objects may be created
from it. 
}
\section{Slots}{
	 \describe{
    \item{\code{type}:}{the type of update to be performed, will be a
    character string }
    \item{\code{mData}:}{a list of information needed to perform the update }
    \item{\code{dataName}:}{the name of the data set to be updated }
  }
}
\section{Extends}{
Class \code{"gModifyMessage"}, directly.
Class \code{"gMessage"}, by class \code{"gModifyMessage"}.
}
\section{Methods}{
No methods defined with class "gUpdateMessage" in the signature.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gMessage-class}},
  \code{\link{gModifyMessage-class}},
  \code{\link{gUpdateViewMessage-class}},
  \code{\link{gUpdateDataMessage-class}} 
}
\examples{
}
\keyword{classes}

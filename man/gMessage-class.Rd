\name{gMessage-class}
\docType{class}
\alias{gMessage-class}
\alias{gMessage}
\alias{handleMessage}
\title{Class "gMessage": A virtual class for messages }
\description{	 
  gMessage is a virtual class from which all other message classes will 
  inherit.  Message objects will be created whenever there is communication
  between the model, view, and controller components or between MVC objects.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
No methods defined with class "gMessage" in the signature.  However, all
message classes will have a handleMessage method that will read the message
and notify the appropriate components so that the information in the message
can be acted upon.  The handleMessage methods will be defined in packages that
use this package, such as iSPlot and iSNetwork.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gUpdateMessage-class}},
  \code{\link{gUpdateViewMessage-class}},
  \code{\link{gUpdateDataMessage-class}},
  \code{\link{gAddMessage-class}},
  \code{\link{gAddViewMessage-class}},
  \code{\link{gAddDataMessage-class}}, 
  \code{\link{gAddChildMessage-class}},
  \code{\link{gUpdateParentMessage-class}},
  \code{\link{gUpdateChildMessage-class}}
}
\examples{
}
\keyword{classes}

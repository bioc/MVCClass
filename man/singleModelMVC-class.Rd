\name{singleModelMVC-class}
\docType{class}
\alias{singleModelMVC-class}
\alias{singleModelMVC}

\title{Class "singleModelMVC": A class to represent a single
model-view-controller object }
\description{
  singleModelMVC is a class to represent a model-view-controller object that
  cannot be linked to other MVC objects (i.e. single refers to the MVC object
  being unlinked).  The singleModelMVC class will combine thee three
  components (model, view and controller) so that the MVC objects can be
  reused. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("singleModelMVC", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{model}:}{the model object (will inherit from the gModel
    virtual class) }
    \item{\code{viewList}:}{a list of the view objects that visualize the
    model }
    \item{\code{controller}:}{the environment that stores information for the
    MVC object }
  }
}
\section{Extends}{
Class \code{"MVC"}, directly.
}
\section{Methods}{
No methods defined with class "singleModelMVC" in the signature.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gModel-class}},
  \code{\link{genView-class}},
  \code{\link{MVC-class}},
  \code{\link{linkedModelMVC-class}}
}
\examples{
}
\keyword{classes}

\name{Sampler-class}
\docType{class}
\alias{Sampler-class}

\title{Class definition of "Sampler" (Umacs package)}
\description{This class is used only internally, in Umacs.}
\section{Objects from the Class}{}
\section{Slots}{
	 \describe{
    \item{\code{params}:}{(\code{"ParameterList"}) A single object containing the parameters of the sampler. }
    \item{\code{all.param.names}:}{(\code{"character"}) names of the parameters that are `random' }
    \item{\code{check.only}:}{(\code{"logical"}) flag to }
    \item{\code{snippets}:}{(\code{"snippetlist"}) Snippets of which sampler is built from }
  }
}
\section{Extends}{}
\section{Methods}{
  \describe{
	 }
}
\references{Kerman, Jouni. Umacs: A Universal Markov Chain Sampler. Technical report, Columbia University, New York.}
\author{Jouni Kerman <jouni@kerman.com>}
\note{}
\seealso{
\code{\link{Umacs-package}} for an overview of the Umacs package.

\code{\link{Sampler}} for how to create a sampler function; see the vignette for details.

Package `rv'.

}
\examples{}
\keyword{iteration}
\details{
Users need only use the function \code{\link{Sampler}}.

}

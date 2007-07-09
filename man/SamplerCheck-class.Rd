\name{SamplerCheck-class}
\docType{class}
\alias{SamplerCheck-class}

\title{Class definition of "SamplerCheck" (Umacs package)}
\description{This class is used only internally, in Umacs.}
\section{Objects from the Class}{}
\section{Slots}{
	 \describe{
    \item{\code{params}:}{Object of class \code{"ParameterList"} ~~ }
    \item{\code{all.param.names}:}{Object of class \code{"character"} ~~ }
    \item{\code{check.only}:}{Object of class \code{"logical"} ~~ }
    \item{\code{snippets}:}{Object of class \code{"snippetlist"} ~~ }
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
This class is a derivative of the \code{\link{Sampler-class}}, used for
doing a test run on the sampler before passing the sampling function to the user.

}

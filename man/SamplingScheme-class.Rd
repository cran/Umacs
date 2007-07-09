\name{SamplingScheme-class}
\docType{class}
\alias{SamplingScheme-class}

\title{Class definition of "SamplingScheme" (Umacs package)}
\description{This class is used only internally, in Umacs.}
\section{Objects from the Class}{}
\section{Slots}{
	 \describe{
    \item{\code{id}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{stemname}:}{Object of class \code{"character"} ~~ }
    \item{\code{basename}:}{Object of class \code{"character"} ~~ }
    \item{\code{savename}:}{Object of class \code{"character"} ~~ }
    \item{\code{impute}:}{Object of class \code{"logical"} ~~ }
    \item{\code{length}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{total.length}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{dimension}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{check.ok}:}{Object of class \code{"logical"} ~~ }
    \item{\code{vector.names}:}{Object of class \code{"character"} ~~ }
    \item{\code{value}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{missing}:}{Object of class \code{"numeric"} ~~ }
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
\note{\code{RawCode} (\code{\link{RawCode-class}}) inherits directly from \code{\link{Parameter-class}}.}
\seealso{
\code{\link{Umacs-package}} for an overview of the Umacs package.

\code{\link{Sampler}} for how to create a sampler function; see the vignette for details.

Package `rv'.

}
\examples{}
\keyword{iteration}
\details{
This is the base class for all model parameters that need a Metropolis-
style updating routine.
\code{SamplingScheme} is the parent class for such classes as
\code{\link{Gibbs}} and \code{\link{Metropolis}}).

}

\name{Parameter-class}
\docType{class}
\alias{Parameter-class}

\title{Class definition of "Parameter" (Umacs package)}
\description{This class is used only internally, in Umacs.}
\section{Objects from the Class}{}
\section{Slots}{
	 \describe{
    \item{\code{id}:}{(\code{"numeric"}) an unique id }
    \item{\code{name}:}{(\code{"character"}) full specification of the parameter name, with brackets and but without suffixes. }
    \item{\code{stemname}:}{(\code{"character"}) `stem' of the name, not including brackets or suffixes, (e.g. ``theta"). }
    \item{\code{basename}:}{(\code{"character"}) the name of the parameter, including brackets, but not including suffixes such as ``.mis" (e.g. "theta[1]") }
    \item{\code{savename}:}{(\code{"character"}) the parameter specification as the user specified it, (e.g. "theta[1].mis") }
    \item{\code{impute}:}{(\code{"logical"}) (not used) }
    \item{\code{length}:}{(\code{"numeric"}) length of the actual 'missing' values: code{length(theta.mis)} }
    \item{\code{total.length}:}{(\code{"numeric"}) length of the 'stemname' vector : code{length(theta)} }
    \item{\code{dimension}:}{(\code{"numeric"})  }
    \item{\code{check.ok}:}{(\code{"logical"}) code{TRUE} if parameter check went ok }
    \item{\code{vector.names}:}{(\code{"character"}) (not used) }
    \item{\code{value}:}{(\code{"numeric"})  }
    \item{\code{missing}:}{(\code{"numeric"})  }
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
This is the base class for all model parameters that need to be
updated within the sampler loop.
\code{Parameter} is the parent class for ``sampling schemes" (\code{\link{SamplingScheme-class}},
which contain such as \code{\link{Gibbs}} and \code{\link{Metropolis}}),
and ``raw code" (\code{\link{RawCode-class}}).

}

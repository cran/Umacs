\name{RawCode}
\alias{RawCode}
\title{Generate a RawCode object for Umacs Sampler function}
\description{
Generates a \code{RawCode} object that is used to initialize an R function
that performs a Gibbs updating set within the sampler function.

}
\usage{
RawCode(update, init = NULL)
}
\arguments{
  \item{update}{ An R function to be executed in the iteration loop}
  \item{init}{ An R function to be executed before the iteration loop (chain); by default no code (code{NULL}) }
}
\details{
\code{RawCode} is to be used only within the \code{Sampler} function call;
the value of the argument (\code{x}) is made available (locally)
in a sampling function that is built using the Umacs function \code{Sampler}

The name of the parameter that will have the value (\code{x})
is \emph{not} specified here, but only within the \code{Sampler} function call.

}
\value{
An object of class \code{RawCode}
to be further processed by the Umacs function \code{Sampler}

}
\references{Kerman, Jouni. Umacs: A Universal Markov Chain Sampler. Technical report, Columbia University, New York.}
\author{Jouni Kerman <jouni@kerman.com>}
\note{
Usually a call to this function is not necessary,
since it is implicitly done when an R function is given in the argument list
of the \code{Sampler} function call without a name.

\code{RawCode} \emph{must} be associated with a parameter name.
Components of vector parameters specified to be \code{RawCode}s cannot be updated,
but those specified to be \code{Data} can be updated.

To specify a local function that is supposed to be available,
use the function \code{LocalFunction}.
}
\seealso{
\code{\link{Umacs-package}} for an overview of the Umacs package.

\code{\link{Sampler}} for how to create a sampler function; see the vignette for details.

Package `rv'.

}
\examples{}
\keyword{iteration}

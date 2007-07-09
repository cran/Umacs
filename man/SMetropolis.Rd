\name{SMetropolis}
\alias{SMetropolis}
\title{Generate an SMetropolis object for the Umacs Sampler function}
\description{
Generates an object including the information necessary
to generate an SMetropolis updating step for a parameter,
within the main loop
of an iterative sampler function.

The sampler is built using the Umacs function \code{\link{Sampler}};
and the arguments (\code{update} and \code{init})
are embedded in the sampler function.

See \code{\link{Sampler}} for further details.

}
\usage{
SMetropolis(logpost = NULL, init, adapt.scale.function = "scaleAdapt.Metropolis", scale.jump = NA, class. = "SMetropolis")
}
\arguments{
  \item{logpost}{ An R function calculating the value (a scalar) of the unnormalized log-posterior function given all other arguments}
  \item{init}{ An R function that returns a (random) starting point (a scalar!) for a Markov chain for the parameter}
  \item{adapt.scale.function}{ name of the function used to adapt the proposal kernel (usually, just ignore and use the default)}
  \item{scale.jump}{ The scale of the proposal kernel matrix, if adaptation is not to be used}
  \item{class.}{ (for internal use only) }
}
\details{
\code{SMetropolis} is a special case of the \code{Metropolis} sampling scheme;
it is optimized for scalar parameters.

\code{SMetropolis} is to be used only within the \code{Sampler} function call.

The arguments of the update and init functions are ignored.
The body of the function can contain references to any parameters
that are defined in the \code{Sampler} function call, or to any
variables in an enclosing environment (such as the Global Environment).

The name of the parameter that is being updated is \emph{not} specified here,
but only within the \code{Sampler} function call.

}
\value{
An object of class \code{SMetropolis},
to be further processed by the Umacs function \code{\link{Sampler}}.
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

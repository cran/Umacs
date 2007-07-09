\name{PSMetropolis}
\alias{PSMetropolis}
\title{Generate an PSMetropolis object for the Umacs Sampler function}
\description{
Generates an object including the information necessary
to generate a Metropolis updating step for a parameter,
within the main loop
of an iterative sampler function.

The sampler is built using the Umacs function \code{Sampler};
and the arguments (\code{update} and \code{init})
are embedded in the sampler function.

See \code{Sampler} for further details.

}
\usage{
PSMetropolis(logpost = NULL, init, proposal = NULL, adapt.scale.function = "scaleAdapt.PMetropolis", scale.jump = NA, class. = "PSMetropolis")
}
\arguments{
  \item{logpost}{ An R function calculating the value (a vector!) of the unnormalized log-posterior function given all other arguments}
  \item{init}{ An R function that returns a (random) starting point (a vector) for a Markov chain for the parameter}
  \item{proposal}{ (not used now)}
  \item{adapt.scale.function}{ name of the function used to adapt the proposal kernel (usually, just ignore and use the default)}
  \item{scale.jump}{ The scale of the proposal kernel matrix, if adaptation is not to be used}
  \item{class.}{ (for internal use only; ignore) }
}
\details{
\code{PSMetropolis} is a special case of the \code{PMetropolis} sampling scheme.
The parameter to be updated is a \emph{vector} consisting of
\emph{independently updateable scalars}.
That is, each component of the vector can be updated independently (``in parallel'')
using a (scalar) Metropolis updating step.

In other words, you would use \code{PSMetropolis}
if conditional on other parameters in the model,
you have independent (exchangeable) vector-valued parameters
that you wish to update using the Metropolis algorithm.

If instead you have conditionally independent \emph{vector-valued} parameters,
that is, a \emph{matrix} of independently updateable rows or columns,
use \code{PMetropolis}.

An alternative to using \code{PSMetropolis} is to define
each component as a separate parameter, but this would be very cumbersome and
require lots of repetitive coding, and the code would not be optimized.

\code{PSMetropolis} is to be used only within the \code{\link{Sampler}} function call.

The arguments of the \code{update} and \code{init} functions are ignored.
The body of the function can contain references to any parameters
that are defined in the \code{Sampler} function call, or to any
variables in an enclosing environment (such as the Global Environment).

The name of the parameter that is being updated is \emph{not} specified here,
but only within the \emph{Sampler} function call.

See the documentation for examples.

}
\value{
An object of class \code{PSMetropolis}
to be further processed by the Umacs function \code{Sampler}.
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

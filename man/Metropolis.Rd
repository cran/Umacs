\name{Metropolis}
\alias{Metropolis}
\title{Generate a Metropolis object for the Umacs Sampler function}
\description{
Generates an object including the information necessary
to generate a Metropolis updating step for a parameter,
within the main loop
of an iterative sampler function.

The sampler is built using the Umacs function \code{\link{Sampler}};
and the arguments (\code{update} and \code{init})
are embedded in the sampler function.

}
\usage{
Metropolis(logpost = NULL, init, adapt.scale.function = "scaleAdapt.Metropolis", scale.jump = NA, kernel.jump = "Gaussian", class. = "Metropolis")
}
\arguments{
  \item{logpost}{ An R function calculating the value (a scalar) of the unnormalized log-posterior function given all other arguments}
  \item{init}{ An R function that returns a (random) starting point (scalar, vector, or array) for a Markov chain for the parameter}
  \item{adapt.scale.function}{ name of the function used to adapt the proposal kernel (usually, just ignore and use the default)}
  \item{scale.jump}{ The scale of the proposal kernel matrix, if adaptation is not to be used}
  \item{kernel.jump}{ Proposal-generating density class. Now, the only possible value is `Gaussian'.}
  \item{class.}{ (For internal use only) }
}
\details{
\code{Metropolis} is to be used only within the \code{Sampler} function call.

The arguments of the update and init functions are ignored.
The body of the function can contain references to any parameters
that are defined in the \code{\link{Sampler}} function call, or to any
variables in an enclosing environment (such as the Global Environment).

The name of the parameter that is being updated is \emph{not} specified here,
but only within the \code{\link{Sampler}} function call.

}
\value{
An object of class \code{Metropolis},
to be further processed by the Umacs function \code{Sampler}.
}
\references{Kerman, Jouni. Umacs: A Universal Markov Chain Sampler. Technical report, Columbia University, New York.}
\author{Jouni Kerman <jouni@kerman.com>}
\note{
If the associated parameter is \emph{scalar-valued}, use \code{\link{SMetropolis}} instead.

}
\seealso{
\code{\link{Umacs-package}} for an overview of the Umacs package.

\code{\link{Sampler}} for how to create a sampler function; see the vignette for details.

Package `rv'.

}
\examples{}
\keyword{iteration}

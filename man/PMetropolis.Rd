\name{PMetropolis}
\alias{PMetropolis}
\title{Generate an PMetropolis object for the Umacs Sampler function}
\description{
Generates an object including the information necessary
to generate a ``Parallel Metropolis" updating step for a parameter,
within the main loop
of an iterative sampler function.

The sampler is built using the Umacs function \code{\link{Sampler}};
and the arguments (\code{update} and \code{init})
are embedded in the sampler function.

}
\usage{
PMetropolis(logpost = NULL, init, proposal = NULL, adapt.scale.function = "scaleAdapt.PMetropolis", scale.jump = NA, class. = "PMetropolis", byCol = FALSE)
}
\arguments{
  \item{logpost}{ An R function calculating the value (a vector!) of the unnormalized log-posterior function given all other arguments}
  \item{init}{ An R function that returns a (random) starting point (a vector) for a Markov chain for the parameter}
  \item{proposal}{ (not used now)}
  \item{adapt.scale.function}{ name of the function used to adapt the proposal kernel (usually, just ignore and use the default)}
  \item{scale.jump}{ The scale of the proposal kernel matrix, if adaptation is not to be used}
  \item{class.}{ (for internal use only; ignore)}
  \item{byCol}{ code{TRUE} if the parameter is a matrix consisting of columns of independent vectors; the default is code{FALSE}: the parameter is a matrix consisting of rows of independent vectors }
}
\details{
The `PMetropolis' is short for `Parallel Metropolis':
the parameter to be updated is a \emph{matrix} consisting of
\emph{rows of independently updateable vectors}.
That is, each row of the matrix can be updated independently (``in parallel'')
using a (vector) Metropolis updating step.
(Alternatively the independent vectors can be in the columns of the matrix,
if the argument \code{byCol} is set to \code{TRUE}.)

In other words, you would use \code{PMetropolis}
if conditional on other parameters in the model,
you have independent (exchangeable) vector-valued parameters
that you wish to update using the Metropolis algorithm.

If instead you have conditionally independent \emph{scalar-valued} parameters,
use \code{PSMetropolis} instead.

An alternative to using \code{PMetropolis} is to define
each row as a separate parameter, but this would be cumbersome and
require lots of repetitive coding, and the code would not be
optimized.

\code{PMetropolis} is to be used only within the \code{\link{Sampler}} function call.

The arguments of the update and init functions are ignored.
The body of the function can contain references to any parameters
that are defined in the \code{Sampler} function call, or to any
variables in an enclosing environment (such as the Global Environment).

The name of the parameter that is being updated is \emph{not} specified here,
but only within the \code{\link{Sampler}} function call.

}
\value{
An object of class \code{PMetropolis},
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

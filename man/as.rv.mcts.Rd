\name{as.rv.mcts}
\alias{as.rv.mcts}
\title{Coerce an `mcts' object to an `rv' object}
\description{Coerce an mcts object (esp. those made with Umacs samplers) to an rv object (those compatible with the package `rv')}
\usage{
as.rv.mcts(obj)
}
\arguments{
  \item{obj}{ An mcts object to coerce}
}
\details{
Class `rv' is a simulation-based random variable class.
The simulations stored inside \code{obj} will be copied into the components of an
`rv' object.
}
\value{An object of class \code{rv}.}
\references{Kerman, Jouni. Umacs: A Universal Markov Chain Sampler. Technical report, Columbia University, New York.}
\author{Jouni Kerman <jouni@kerman.com>}
\note{Requires package `rv'.}
\seealso{
\code{\link{Umacs-package}} for an overview of the Umacs package.

\code{\link{Sampler}} for how to create a sampler function; see the vignette for details.

Package `rv'.

}
\examples{}
\keyword{manip}

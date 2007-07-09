\name{as.bugs.mcts}
\alias{as.bugs.mcts}
\title{Coerce an `mcts' object to a `bugs' object}
\description{Coerce an mcts object (esp. those made with Umacs samplers) to an rv object (those compatible with the package `rv')}
\usage{
as.bugs.mcts(x)
}
\arguments{
  \item{x}{ An mcts object to coerce}
}
\details{
Class `bugs' is a simulation-based random variable class.
The simulations stored inside \code{obj} will be copied into the components of an
`rv' object.
}
\value{An object of class \code{rv}.}
\references{Kerman, Jouni. Umacs: A Universal Markov Chain Sampler. Technical report, Columbia University, New York.}
\author{Jouni Kerman <jouni@kerman.com>}
\note{Requires package `rv'.}
\seealso{Package \code{R2WinBUGS}.}
\examples{}
\keyword{manip}

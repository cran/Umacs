\name{mcts}
\alias{mcts}
\alias{print.mcts}
\alias{summary.mcts}
\alias{as.matrix.mcts}
\title{mcts Objects}
\description{
A wrapper for new('mcts', ...)
}
\usage{
mcts(chains, n.burnin = 0, n.chains = numeric(0), n.iter = numeric(0), n.sims = n.iter, col.names = character(0), save.all = TRUE)
\method{print}{mcts}(x, \dots)
\method{summary}{mcts}(object, \dots)
\method{as.matrix}{mcts}(x, \dots)
}
\arguments{
  \item{chains}{ }
  \item{n.burnin}{ }
  \item{n.chains}{ }
  \item{n.iter}{ }
  \item{n.sims}{ }
  \item{col.names}{ }
  \item{save.all}{  }
  \item{x}{mcts object}
  \item{object}{mcts object}
  \item{\dots}{Other arguments}
}
\details{}
\value{An object of the class \code{mcts} (\code{\link{mcts-class}}).}
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

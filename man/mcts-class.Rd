\name{mcts-class}
\docType{class}
\alias{mcts-class}

\title{Class `mcts' (``Markov Chain Time Series'')}
\description{
mcts = "Markov Chain Time Series"; a wrapper for holding
matrix of simulation of MCMC chains along with relevant information.
Umacs returns simulations in the form of \code{mcts} objects.
}
\section{Objects from the Class}{Objects can be created using \code{mcts(...)}}
\section{Slots}{
	 \describe{
    \item{\code{chains}:}{(\code{"list"})  }
    \item{\code{n.burnin}:}{(\code{"numeric"})  }
    \item{\code{n.iter}:}{(\code{"numeric"})  }
    \item{\code{n.sims}:}{(\code{"numeric"})  }
    \item{\code{save.all}:}{(\code{"logical"})  }
    \item{\code{col.names}:}{(\code{"character"})  }
    \item{\code{summary}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Extends}{}
\section{Methods}{
  \describe{
	 }
}
\references{Kerman, Jouni. Umacs: A Universal Markov Chain Sampler. Technical report, Columbia University, New York.}
\author{Jouni Kerman <jouni@kerman.com>}
\note{
\code{mcts} objects can be coerced into random variable (rv) objects using
\code{as.rv}. See package `rv' for details.
}
\seealso{
\code{\link{Umacs-package}} for an overview of the Umacs package.

\code{\link{Sampler}} for how to create a sampler function; see the vignette for details.

Package `rv'.

}
\examples{}
\keyword{iteration}

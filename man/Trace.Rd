\name{Trace}
\alias{Trace}
\title{Generate a Trace object for Umacs Sampler function}
\description{
Generates an object including the information necessary
to show a trace plot of a scalar parameter in a Umacs sampling function.

The sampler is built using the Umacs function \code{\link{Sampler}};
this function is only to be used within the \code{\link{Sampler}} function call.

}
\usage{
Trace(name, label = name, ylim = NULL)
}
\arguments{
  \item{name}{ Name of the scalar parameter to be traced, e.g. "code{theta[1]}".}
  \item{label}{ Label to be shown in the trace plot window. By default the same as code{name}.}
  \item{ylim}{ Range of the vertical axis. By default, the range is extended as necessary to display the highs and the lows of the trace. }
}
\details{
If \code{ylim} is not specified (if it is \code{NULL}), the range of the vertical
axis is adjusted automatically.

\code{Trace} is to be used only within the \code{\link{Sampler}} function call.

}
\value{
An object of class \code{Trace},
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

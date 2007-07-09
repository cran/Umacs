
# Bioassay
# Umacs sample file 
# July 13, 2005 

# This demo file fits the same model four times using three different
# versions of the Metropolis sampling schemes that are found in Umacs.
# For details of the model, see BDA, 2nd ed., pp. 88-93 (in the book it's fit using
# a grid but here for demonstration purposes we'll use Metropolis.)
# 
# Each component of (alpha,beta) is fit with independent Scalar Metropolis sampling schemes;
# Delta ~ (alpha,beta) and is fit using a (vector) Metropolis scheme;
# Theta[1,1:2] ~ (alpha,beta) and theta[2,1:2] ~ (alpha,beta);
# both rows of theta are thus independent and are fit simultaneously
# using the Parallel Vector Metropolis sampling scheme.
#
# The Traces plot the "alpha" traces of the four different fits
# (they should be more or less the same.)


bioassay.ab.logpost <- function()
{
  z <- alpha+beta*x
  sum(y*z - n*log(1+exp(z))) # Assuming the prior is proportional to 1 (log of it is 0)
}

bioassay.theta.logpost <- function()
{
  z <- theta[1,1]+theta[1,2]*x
  a1 <- sum(y*z - n*log(1+exp(z))) # Assuming the prior is proportional to 1 (log of it is 0)
  z <- theta[2,1]+theta[2,2]*x
  a2 <- sum(y*z - n*log(1+exp(z))) # Assuming the prior is proportional to 1 (log of it is 0)
  c(a1,a2)
}

bioassay.delta.logpost <- function()
{
  z <- delta[1]+delta[2]*x
  sum(y*z - n*log(1+exp(z))) # Assuming the prior is proportional to 1 (log of it is 0)
}


s <- Sampler(
  .title  = "Bioassay example, Metropolis sampling demo",
  .logpost = bioassay.ab.logpost,
  y = c(0, 1, 3, 5),                # number of observed deaths
  n = c(5, 5, 5, 5),                # number of animals in treatment group
  x = c(-0.86, -0.30, -0.05, 0.73), # (log of) the treatment dose
  theta  = PMetropolis(bioassay.theta.logpost, init=function () matrix(rnorm(4, 0, 1),2)),
  alpha  = SMetropolis(init=function () rnorm(1, 0, 1)),
  beta   = SMetropolis(init=function () rnorm(1, 0, 1)),
  delta  = Metropolis(bioassay.delta.logpost, init=function () rnorm(2, 0, 1)),
  Trace('theta[1,1]'),
  Trace('theta[2,2]'),
  Trace('alpha'),
  Trace('delta[1]')
)


a <- s(200, n.sims=200)
library("rv")
rvattach(as.rv(a))

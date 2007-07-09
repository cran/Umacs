
# 8 schools
# Bayesian Data Analysis, 2nd ed., (Gelman, Carlin, Stern, Rubin)
# 
# Umacs sample file 
# July 13, 2005 


theta.init <- function() rep(0,J)
mu.init <- function() rnorm(1,mean(y),sd(y))
tau.init <- function() runif(1,0,sd(y))
theta.update <- function() {
  V.theta   <- 1/(1/tau^2 + 1/sigma.y^2)
  theta.hat <- (mu/tau^2 + y/sigma.y^2) * V.theta
  rnorm(J, theta.hat, sqrt(V.theta))
}
mu.update <- function() rnorm(1, mean(theta), tau/sqrt(J))
tau.update <- function() sqrt(sum((theta-mu)^2)/rchisq(1,J-1))

s <- Sampler(
  .title   = "Eight Schools Problem, Gibbs sampling",
  J       = 8,
  sigma.y = c(15, 10, 16, 11,  9, 11, 10, 18),
  y       = c(28,  8, -3,  7, -1,  1, 18, 12),
  theta   = Gibbs(theta.update,theta.init),
  mu      = Gibbs(mu.update,mu.init),
  tau     = Gibbs(tau.update, tau.init),
  Trace('theta[1]'),
  Trace('mu'),
  Trace('tau')
)

a <- s(n.iter=1000)
library("rv")
rvattach(as.rv(a))

print(a)
cat("Now you can try accessing theta, mu, tau, directly.\n")
cat("Type 'a' for a quick summary of all variables\n")

# end

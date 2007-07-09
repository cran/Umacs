
# 8 schools problem with the t distribution 
# Umacs sample file 
# July 13, 2005 


theta.init  <- function () rep(0,J)
theta.init  <- function () rnorm(J)

V.init      <- function () runif(J, 0, sd(y))^2
mu.init     <- function () rnorm(1,mean(y),sd(y))
tau.init    <- function () runif(1,0,sd(y))
nu.inv.init <- function () runif(1)

log.post.nu.inv <- function () {
  nu <- 1/nu.inv
  if (nu.inv<=0 || nu.inv>1) return(-Inf)
  sum( 0.5*nu*log(nu/2) + nu*log(tau) -
    lgamma(nu/2) - (1+nu/2)*log(V) - 0.5*nu*tau^2/V)
}

theta.update <- function () {
  V.theta <- 1/(1/V + 1/sigma.y^2)
  theta.hat <- (mu/V + y/sigma.y^2) * V.theta
  rnorm(J, theta.hat, sqrt(V.theta))
}

mu.update   <- function () {
  V.mu <- 1/sum(1/V)
  mu.hat <- sum(theta/V)*V.mu
  rnorm(1, mu.hat, sqrt(V.mu))
}

V.update <- function () {
  (nu*tau^2 + (theta-mu)^2)/rchisq(J, nu+1)
}

tau.update  <- function() {
  sqrt(rgamma(1, 1+J*nu/2, (nu/2)*sum(1/V)))
}

s <- Sampler(
  .title   = "Eight Schools Problem, t model with varying degrees of freedom",
  J       = 8,
  sigma.y = c(15, 10, 16, 11,  9, 11, 10, 18),
  y       = c(28,  8, -3,  7, -1,  1, 18, 12),
  theta   = Gibbs(theta.update,theta.init),
  V       = Gibbs(V.update, V.init),
  mu      = Gibbs(mu.update,mu.init),
  tau     = Gibbs(tau.update, tau.init),
  nu.inv  = SMetropolis(log.post.nu.inv, nu.inv.init),
  nu      = Gibbs(function () 1/nu.inv, function () 1),
  Trace('theta[1]'),
  Trace('mu'),
  Trace('nu')
)

a <- s(n.iter=2000)
library("rv")
rvattach(as.rv(a))

print(a)

cat("Now you can try accessing theta, V, mu, tau, nu.inv, nu, directly,\n")
cat("You can even type: sqrt(V), etc.\n")
cat("Or type 'a' which contains a summary of all variables\n")

# end

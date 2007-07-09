
#
# 8 schools problem with the t distribution, with school 1 missing
# Umacs sample file 
# Feb 02, 2006.
#


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

y.mis.update <- function () rnorm(length(y.NA), mean=theta[y.NA], sd=sigma.y[y.NA])
y.mis.init   <- function () rnorm(length(y.NA), mean=mean(y[-y.NA]), sd=sd(y[-y.NA]))

s <- Sampler(
  .title   = "Eight Schools Problem, t model with varying degrees of freedom",
  J       = 8,
  sigma.y = c(15, 10, 16, 11,  9, 11, 10, 18),
  y       = c(NA,  8, -3,  7, -1,  1, 18, 12),
  y.mis   = Gibbs(y.mis.update, y.mis.init),
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

a <- s(n.iter=100)
library("rv")
y <- c(NA,  8, -3,  7, -1,  1, 18, 12)
rvattach(as.rv(a), impute=TRUE)

print(a)

cat("Now you can try accessing y, theta, V, mu, tau, nu.inv, nu, directly,\n")
cat("Note that 'y[1]' was imputed, so 'y' is now a random vector.\n")
cat("Type 'a' to see a summary of all variables\n")

# end

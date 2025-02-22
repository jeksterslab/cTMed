% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cTMed-methods-ctmedboot.R
\name{confint.ctmedboot}
\alias{confint.ctmedboot}
\title{Bootstrap Method Confidence Intervals}
\usage{
\method{confint}{ctmedboot}(object, parm = NULL, level = 0.95, type = "pc", ...)
}
\arguments{
\item{object}{Object of class \code{ctmedboot}.}

\item{parm}{a specification of which parameters
are to be given confidence intervals,
either a vector of numbers or a vector of names.
If missing, all parameters are considered.}

\item{level}{the confidence level required.}

\item{type}{Charater string.
Confidence interval type, that is,
\code{type = "pc"} for percentile;
\code{type = "bc"} for bias corrected.}

\item{...}{additional arguments.}
}
\value{
Returns a data frame of confidence intervals.
}
\description{
Bootstrap Method Confidence Intervals
}
\examples{
\dontrun{
library(bootStateSpace)
# prepare parameters
## number of individuals
n <- 50
## time points
time <- 100
delta_t <- 0.10
## dynamic structure
p <- 3
mu0 <- rep(x = 0, times = p)
sigma0 <- matrix(
  data = c(
    1.0,
    0.2,
    0.2,
    0.2,
    1.0,
    0.2,
    0.2,
    0.2,
    1.0
  ),
  nrow = p
)
sigma0_l <- t(chol(sigma0))
mu <- rep(x = 0, times = p)
phi <- matrix(
  data = c(
    -0.357,
    0.771,
    -0.450,
    0.0,
    -0.511,
    0.729,
    0,
    0,
    -0.693
  ),
  nrow = p
)
sigma <- matrix(
  data = c(
    0.24455556,
    0.02201587,
    -0.05004762,
    0.02201587,
    0.07067800,
    0.01539456,
    -0.05004762,
    0.01539456,
    0.07553061
  ),
  nrow = p
)
sigma_l <- t(chol(sigma))
## measurement model
k <- 3
nu <- rep(x = 0, times = k)
lambda <- diag(k)
theta <- 0.2 * diag(k)
theta_l <- t(chol(theta))

boot <- PBSSMOUFixed(
  R = 1000L,
  path = getwd(),
  prefix = "ou",
  n = n,
  time = time,
  delta_t = delta_t,
  mu0 = mu0,
  sigma0_l = sigma0_l,
  mu = mu,
  phi = phi,
  sigma_l = sigma_l,
  nu = nu,
  lambda = lambda,
  theta_l = theta_l,
  ncores = parallel::detectCores() - 1,
  seed = 42
)
phi_hat <- phi
colnames(phi_hat) <- rownames(phi_hat) <- c("x", "m", "y")
phi <- extract(object = boot, what = "phi")

# Specific time interval ----------------------------------------------------
boot <- BootMed(
  phi = phi,
  phi_hat = phi_hat,
  delta_t = 1,
  from = "x",
  to = "y",
  med = "m"
)
confint(boot)
confint(boot, type = "bc") # bias-corrected

# Range of time intervals ---------------------------------------------------
boot <- BootMed(
  phi = phi,
  phi_hat = phi_hat,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m"
)
confint(boot)
confint(boot, type = "bc") # bias-corrected
}

}
\author{
Ivan Jacob Agaloos Pesigan
}
\keyword{methods}

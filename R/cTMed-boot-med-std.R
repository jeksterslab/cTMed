#' Bootstrap Sampling Distribution
#' of Standardized Total, Direct, and Indirect Effects
#' of X on Y Through M
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function generates a bootstrap method
#' sampling distribution
#' of the standardized total, direct and indirect effects
#' of the independent variable \eqn{X}
#' on the dependent variable \eqn{Y}
#' through mediator variables \eqn{\mathbf{m}}
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
#' using the first-order stochastic differential equation model
#' drift matrix \eqn{\boldsymbol{\Phi}}.
#'
#' @details See [TotalStd()],
#'   [DirectStd()], and
#'   [IndirectStd()] for more details.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigma List of numeric matrices.
#'   Each element of the list is a bootstrap estimate
#'   of the process noise covariance matrix
#'   (\eqn{\boldsymbol{\Sigma}}).
#' @param sigma_hat Numeric matrix.
#'   The estimated process noise covariance matrix
#'   (\eqn{\hat{\boldsymbol{\Sigma}}})
#'   from the original data set.
#' @inheritParams BootMed
#' @inherit BootMed references
#'
#' @return Returns an object
#'   of class `ctmedboot` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("BootMedStd").}
#'     \item{output}{A list with length of `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{est}{A vector of standardized total, direct, and indirect effects.}
#'     \item{thetahatstar}{A matrix of bootstrap
#'     standardized total, direct, and indirect effects.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(bootStateSpace)
#' # prepare parameters
#' ## number of individuals
#' n <- 50
#' ## time points
#' time <- 100
#' delta_t <- 0.10
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- matrix(
#'   data = c(
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0
#'   ),
#'   nrow = p
#' )
#' sigma0_l <- t(chol(sigma0))
#' mu <- rep(x = 0, times = p)
#' phi <- matrix(
#'   data = c(
#'     -0.357,
#'     0.771,
#'     -0.450,
#'     0.0,
#'     -0.511,
#'     0.729,
#'     0,
#'     0,
#'     -0.693
#'   ),
#'   nrow = p
#' )
#' sigma <- matrix(
#'   data = c(
#'     0.24455556,
#'     0.02201587,
#'     -0.05004762,
#'     0.02201587,
#'     0.07067800,
#'     0.01539456,
#'     -0.05004762,
#'     0.01539456,
#'     0.07553061
#'   ),
#'   nrow = p
#' )
#' sigma_l <- t(chol(sigma))
#' ## measurement model
#' k <- 3
#' nu <- rep(x = 0, times = k)
#' lambda <- diag(k)
#' theta <- 0.2 * diag(k)
#' theta_l <- t(chol(theta))
#'
#' boot <- PBSSMOUFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = getwd(),
#'   prefix = "ou",
#'   n = n,
#'   time = time,
#'   delta_t = delta_t,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   mu = mu,
#'   phi = phi,
#'   sigma_l = sigma_l,
#'   nu = nu,
#'   lambda = lambda,
#'   theta_l = theta_l,
#'   ncores = NULL, # consider using multiple cores
#'   seed = 42
#' )
#' phi_hat <- phi
#' colnames(phi_hat) <- rownames(phi_hat) <- c("x", "m", "y")
#' sigma_hat <- sigma
#' phi <- extract(object = boot, what = "phi")
#' sigma <- extract(object = boot, what = "sigma")
#'
#' # Specific time interval ----------------------------------------------------
#' BootMedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   phi_hat = phi_hat,
#'   sigma_hat = sigma_hat,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' boot <- BootMedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   phi_hat = phi_hat,
#'   sigma_hat = sigma_hat,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(boot)
#' plot(boot, type = "bc") # bias-corrected
#'
#' # Methods -------------------------------------------------------------------
#' # BootMedStd has a number of methods including
#' # print, summary, confint, and plot
#' print(boot)
#' summary(boot)
#' confint(boot, level = 0.95)
#' print(boot, type = "bc") # bias-corrected
#' summary(boot, type = "bc")
#' confint(boot, level = 0.95, type = "bc")
#' }
#'
#' @family Continuous-Time Mediation Functions
#' @keywords cTMed path boot
#' @export
BootMedStd <- function(phi,
                       sigma,
                       phi_hat,
                       sigma_hat,
                       delta_t,
                       from,
                       to,
                       med,
                       ncores = NULL,
                       tol = 0.01) {
  idx <- rownames(phi_hat)
  stopifnot(
    idx == colnames(phi_hat),
    length(from) == 1,
    length(to) == 1,
    from %in% idx,
    to %in% idx
  )
  for (i in seq_len(length(med))) {
    stopifnot(
      med[i] %in% idx
    )
  }
  args <- list(
    phi = phi,
    sigma = sigma,
    phi_hat = phi_hat,
    sigma_hat = sigma_hat,
    delta_t = delta_t,
    from = from,
    to = to,
    med = med,
    ncores = ncores,
    method = "boot",
    network = FALSE
  )
  delta_t <- sort(
    ifelse(
      test = delta_t < tol,
      yes = tol, # .Machine$double.xmin
      no = delta_t
    )
  )
  from <- which(idx == from)
  to <- which(idx == to)
  med <- sapply(
    X = med,
    FUN = function(x,
                   idx) {
      which(idx == x)
    },
    idx = idx
  )
  output <- .BootMedStd(
    phi = phi,
    sigma = sigma,
    phi_hat = phi_hat,
    sigma_hat = sigma_hat,
    delta_t = delta_t,
    from = from,
    to = to,
    med = med,
    ncores = ncores
  )
  names(output) <- delta_t
  out <- list(
    call = match.call(),
    args = args,
    fun = "BootMedStd",
    output = output
  )
  class(out) <- c(
    "ctmedboot",
    class(out)
  )
  out
}

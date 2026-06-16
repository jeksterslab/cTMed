#' Posterior Distribution
#' of Standardized Total, Direct, and Indirect Effects
#' of X on Y Through M
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function generates a posterior
#' distribution
#' of the standardized total, direct, and indirect effects
#' of the independent variable \eqn{X}
#' on the dependent variable \eqn{Y}
#' through mediator variables \eqn{\mathbf{m}}
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
#' using the posterior distribution
#' of the first-order stochastic differential equation model
#' drift matrix \eqn{\boldsymbol{\Phi}} and
#' process noise covariance matrix \eqn{\boldsymbol{\Sigma}}.
#'
#' @details See [TotalStd()],
#'   [DirectStd()], and
#'   [IndirectStd()] for more details.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams PosteriorMed
#' @param sigma List of numeric matrices.
#'   Each element of the list is a sample
#'   from the posterior distribution
#'   of the process noise covariance matrix
#'   (\eqn{\boldsymbol{\Sigma}}).
#'   Each matrix should have row and column names
#'   pertaining to the variables in the system.
#'
#' @inherit IndirectStd references
#'
#' @return Returns an object
#'   of class `ctmedmc` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("PosteriorMedStd").}
#'     \item{output}{A list of length `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{est}{Mean of the posterior distribution
#'     of the standardized total, direct, and indirect effects.}
#'     \item{thetahatstar}{Posterior distribution of the
#'     standardized total, direct, and indirect effects.}
#'   }
#'
#' @examples
#' set.seed(42)
#' phi <- matrix(
#'   data = c(
#'     -0.357, 0.771, -0.450,
#'     0.000, -0.511, 0.729,
#'     0.000, 0.000, -0.693
#'   ),
#'   nrow = 3
#' )
#' colnames(phi) <- rownames(phi) <- c("x", "m", "y")
#' sigma <- matrix(
#'   data = c(
#'     0.24455556, 0.02201587, -0.05004762,
#'     0.02201587, 0.07067800, 0.01539456,
#'     -0.05004762, 0.01539456, 0.07553061
#'   ),
#'   nrow = 3
#' )
#' colnames(sigma) <- rownames(sigma) <- c("x", "m", "y")
#' input <- MCPhiSigma(
#'   phi = phi,
#'   sigma = sigma,
#'   vcov_theta = 0.001 * diag(15),
#'   R = 100L,
#'   seed = 42
#' )$output
#' phi <- lapply(
#'   X = input,
#'   FUN = function(x) {
#'     x[[1]]
#'   }
#' )
#' sigma <- lapply(
#'   X = input,
#'   FUN = function(x) {
#'     x[[2]]
#'   }
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' PosteriorMedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' posterior <- PosteriorMedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(posterior)
#'
#' # Methods -------------------------------------------------------------------
#' # PosteriorMedStd has a number of methods including
#' # print, summary, confint, and plot
#' print(posterior)
#' summary(posterior)
#' confint(posterior, level = 0.95)
#' plot(posterior)
#'
#' @family Continuous-Time Mediation Functions
#' @keywords cTMed path posterior
#' @export
PosteriorMedStd <- function(phi,
                            sigma,
                            delta_t,
                            from,
                            to,
                            med,
                            ncores = NULL,
                            tol = 0.001) {
  stopifnot(
    is.list(phi),
    is.list(sigma),
    length(phi) == length(sigma),
    length(phi) > 0,
    is.matrix(phi[[1]]),
    is.matrix(sigma[[1]])
  )
  idx <- rownames(phi[[1]])
  stopifnot(
    idx == colnames(phi[[1]]),
    idx == rownames(sigma[[1]]),
    idx == colnames(sigma[[1]]),
    length(from) == 1,
    length(to) == 1,
    from %in% idx,
    to %in% idx
  )
  for (i in seq_along(med)) {
    stopifnot(
      med[i] %in% idx
    )
  }
  delta_t <- sort(
    unique(
      pmax(
        delta_t,
        tol
      )
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
  args <- list(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t,
    from = from,
    to = to,
    med = med,
    ncores = ncores,
    method = "posterior",
    network = FALSE,
    standardized = TRUE
  )
  output <- .PosteriorMedStd(
    phi = phi,
    sigma = sigma,
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
    fun = "PosteriorMedStd",
    output = output
  )
  class(out) <- c(
    "ctmedmc",
    class(out)
  )
  out
}

#' Posterior Distribution
#' of the Standardized Indirect Effect Centrality
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function generates a posterior
#' distribution
#' of the standardized indirect effect centrality
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
#' using the posterior distribution
#' of the first-order stochastic differential equation model
#' drift matrix \eqn{\boldsymbol{\Phi}} and
#' process noise covariance matrix \eqn{\boldsymbol{\Sigma}}.
#'
#' @details See [IndirectCentralStd()] for more details.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams PosteriorTotalCentralStd
#'
#' @inherit Indirect references
#'
#' @return Returns an object
#'   of class `ctmedmc` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("PosteriorIndirectCentralStd").}
#'     \item{output}{A list of length `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{est}{Mean of the posterior distribution
#'     of the standardized indirect effect centrality.}
#'     \item{thetahatstar}{Posterior distribution of the
#'     standardized indirect effect centrality measure.}
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
#'
#' sigma <- matrix(
#'   data = c(
#'     0.24455556, 0.02201587, -0.05004762,
#'     0.02201587, 0.07067800, 0.01539456,
#'     -0.05004762, 0.01539456, 0.07553061
#'   ),
#'   nrow = 3
#' )
#' colnames(sigma) <- rownames(sigma) <- c("x", "m", "y")
#'
#' input <- MCPhiSigma(
#'   phi = phi,
#'   sigma = sigma,
#'   vcov_theta = 0.001 * diag(15),
#'   R = 100L,
#'   seed = 42
#' )$output
#'
#' phi <- lapply(
#'   X = input,
#'   FUN = function(x) {
#'     x[[1]]
#'   }
#' )
#'
#' sigma <- lapply(
#'   X = input,
#'   FUN = function(x) {
#'     x[[2]]
#'   }
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' PosteriorIndirectCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' posterior <- PosteriorIndirectCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:5
#' )
#'
#' # Methods -------------------------------------------------------------------
#' # PosteriorIndirectCentralStd has a number of methods including
#' # print, summary, confint, and plot
#' print(posterior)
#' summary(posterior)
#' confint(posterior, level = 0.95)
#' plot(posterior)
#'
#' @family Continuous-Time Mediation Functions
#' @keywords cTMed network posterior
#' @export
PosteriorIndirectCentralStd <- function(phi,
                                        sigma,
                                        delta_t,
                                        ncores = NULL,
                                        tol = 0.001) {
  stopifnot(
    is.list(phi),
    is.list(sigma),
    length(phi) == length(sigma),
    is.matrix(phi[[1]]),
    is.matrix(sigma[[1]])
  )
  type <- "indirect"
  delta_t <- sort(
    unique(
      pmax(
        delta_t,
        tol
      )
    )
  )
  args <- list(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t,
    ncores = ncores,
    method = "posterior",
    network = TRUE,
    type = type,
    standardized = TRUE
  )
  output <- .PosteriorCentralStd(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t,
    type = type,
    ncores = ncores
  )
  names(output) <- delta_t
  out <- list(
    call = match.call(),
    args = args,
    fun = "PosteriorIndirectCentralStd",
    output = output
  )
  class(out) <- c(
    "ctmedmc",
    class(out)
  )
  out
}

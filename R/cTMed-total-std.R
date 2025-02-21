#' Standardized Total Effect Matrix
#' Over a Specific Time Interval
#'
#' This function computes the standardized total effects matrix
#' over a specific time interval \eqn{\Delta t}
#' using the first-order stochastic differential equation model's
#' drift matrix \eqn{\boldsymbol{\Phi}}
#' and process noise covariance matrix \eqn{\boldsymbol{\Sigma}}.
#'
#' @details The standardized total effect matrix
#'   over a specific time interval \eqn{\Delta t}
#'   is given by
#'   \deqn{
#'     \mathrm{Total}^{\ast}_{{\Delta t}_{i, j}}
#'     =
#'     \mathrm{Total}_{{\Delta t}_{i, j}}
#'     \left(
#'     \frac{\sigma_{{x}_{j}}}{\sigma_{{y}_{i}}}
#'     \right)
#'   }
#'   where
#'   \eqn{\boldsymbol{\Phi}} denotes the drift matrix,
#'   \eqn{\sigma_{{x}_{j}}} and \eqn{\sigma_{{y}_{i}}}
#'   are the steady-state model-implied standard deviations
#'   of the state independent and dependent variables, respectively, and
#'   \eqn{\Delta t} the time interval.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams IndirectStd
#' @inherit IndirectStd references
#'
#' @return Returns an object
#'   of class `ctmedeffect` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("TotalStd").}
#'     \item{output}{The standardized matrix of total effects.}
#'   }
#'
#' @examples
#' phi <- matrix(
#'   data = c(
#'     -0.357, 0.771, -0.450,
#'     0.0, -0.511, 0.729,
#'     0, 0, -0.693
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
#' delta_t <- 1
#' TotalStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = delta_t
#' )
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed effects
#' @export
TotalStd <- function(phi,
                     sigma,
                     delta_t) {
  idx <- rownames(phi)
  stopifnot(
    idx == colnames(phi)
  )
  args <- list(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t
  )
  output <- .TotalStd(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t
  )
  colnames(output) <- rownames(output) <- idx
  out <- list(
    call = match.call(),
    args = args,
    fun = "TotalStd",
    output = output
  )
  class(out) <- c(
    "ctmedeffect",
    class(out)
  )
  return(out)
}

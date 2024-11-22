#' Standardized Total Effect Matrix
#' Over a Specific Time Interval
#'
#' This function computes the total effects matrix
#' over a specific time interval \eqn{\Delta t}
#' using the first-order stochastic differential equation model's
#' drift matrix \eqn{\boldsymbol{\Phi}}
#' and process noise covariance matrix \eqn{\boldsymbol{\Sigma}}.
#'
#' @details The standardized total effect matrix
#'   over a specific time interval \eqn{\Delta t}
#'   is given by
#'   \deqn{
#'     \mathrm{Total}^{\ast}_{\Delta t}
#'     =
#'     \mathbf{S}
#'     \left(
#'     \exp
#'     \left(
#'       \Delta t
#'       \boldsymbol{\Phi}
#'     \right)
#'     \right)
#'     \mathbf{S}^{-1}
#'   }
#'   where
#'   \eqn{\boldsymbol{\Phi}} denotes the drift matrix,
#'   \eqn{\mathbf{S}} a diagonal matrix with model-implied
#'   standard deviations on the diagonals and
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
#'     \item{output}{The matrix of total effects.}
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
#'     0.24, 0.02, -0.05,
#'     0.02, 0.07, 0.02,
#'     -0.05, 0.02, 0.08
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

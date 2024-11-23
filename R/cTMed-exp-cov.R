#' Model-Implied State Covariance Matrix
#'
#' The function returns the model-implied state covariance matrix
#' for a particular time interval \eqn{\Delta t}
#' given by
#' \deqn{
#'   \mathrm{vec} \left( \mathrm{Cov}_{\boldsymbol{\eta}} \right)
#'   =
#'   \left(
#'     \mathbf{J} -
#'     \boldsymbol{\beta}_{\Delta t} \otimes \boldsymbol{\beta}_{\Delta t}
#'   \right)^{-1}
#'   \mathrm{vec} \left( \boldsymbol{\Psi}_{\Delta t} \right)
#' }
#' where
#' \deqn{
#'   \boldsymbol{\beta}_{\Delta t}
#'   =
#'   \exp \left( \Delta t \boldsymbol{\Phi} \right) ,
#' }
#' \deqn{
#'   \boldsymbol{\Psi}_{\Delta t}
#'   =
#'   \boldsymbol{\Phi}^{\#}
#'   \left(
#'     \exp \left( \Delta t \boldsymbol{\Phi} \right) - \mathbf{J}
#'   \right)
#' \mathrm{vec} \left( \boldsymbol{\Sigma} \right) , \quad \mathrm{and}
#' }
#' \deqn{
#'   \boldsymbol{\Phi}^{\#}
#'   =
#'   \left(
#'     \boldsymbol{\Phi} \otimes \mathbf{I}
#'   \right) +
#'   \left(
#'     \mathbf{I} \otimes \boldsymbol{\Phi}
#'   \right).
#' }
#' Note that \eqn{\mathbf{I}} and \eqn{\mathbf{J}} are identity matrices.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams IndirectStd
#'
#' @return Returns a numeric matrix.
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
#' ExpCov(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = delta_t
#' )
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed expectations
#' @export
ExpCov <- function(phi,
                   sigma,
                   delta_t) {
  output <- .TotalCov(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t
  )
  colnames(output) <- colnames(phi)
  rownames(output) <- rownames(phi)
  return(output)
}

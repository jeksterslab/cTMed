#' Model-Implied State Covariance Matrix
#'
#' The function returns the model-implied state covariance matrix
#' for a particular time interval \eqn{\Delta t}
#' given by
#' \deqn{
#'   \mathrm{vec} \left( \mathrm{Cov} \left( \boldsymbol{\eta} \right) \right)
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
#' @details
#'   ## Linear Stochastic Differential Equation Model
#'
#'   The measurement model is given by
#'   \deqn{
#'     \mathbf{y}_{i, t}
#'     =
#'     \boldsymbol{\nu}
#'     +
#'     \boldsymbol{\Lambda}
#'     \boldsymbol{\eta}_{i, t}
#'     +
#'     \boldsymbol{\varepsilon}_{i, t},
#'     \quad
#'     \mathrm{with}
#'     \quad
#'     \boldsymbol{\varepsilon}_{i, t}
#'     \sim
#'     \mathcal{N}
#'     \left(
#'     \mathbf{0},
#'     \boldsymbol{\Theta}
#'     \right)
#'   }
#'   where
#'   \eqn{\mathbf{y}_{i, t}},
#'   \eqn{\boldsymbol{\eta}_{i, t}},
#'   and
#'   \eqn{\boldsymbol{\varepsilon}_{i, t}}
#'   are random variables
#'   and
#'   \eqn{\boldsymbol{\nu}},
#'   \eqn{\boldsymbol{\Lambda}},
#'   and
#'   \eqn{\boldsymbol{\Theta}}
#'   are model parameters.
#'   \eqn{\mathbf{y}_{i, t}}
#'   represents a vector of observed random variables,
#'   \eqn{\boldsymbol{\eta}_{i, t}}
#'   a vector of latent random variables,
#'   and
#'   \eqn{\boldsymbol{\varepsilon}_{i, t}}
#'   a vector of random measurement errors,
#'   at time \eqn{t} and individual \eqn{i}.
#'   \eqn{\boldsymbol{\nu}}
#'   denotes a vector of intercepts,
#'   \eqn{\boldsymbol{\Lambda}}
#'   a matrix of factor loadings,
#'   and
#'   \eqn{\boldsymbol{\Theta}}
#'   the covariance matrix of
#'   \eqn{\boldsymbol{\varepsilon}}.
#'
#'   An alternative representation of the measurement error
#'   is given by
#'   \deqn{
#'     \boldsymbol{\varepsilon}_{i, t}
#'     =
#'     \boldsymbol{\Theta}^{\frac{1}{2}}
#'     \mathbf{z}_{i, t},
#'     \quad
#'     \mathrm{with}
#'     \quad
#'     \mathbf{z}_{i, t}
#'     \sim
#'     \mathcal{N}
#'     \left(
#'     \mathbf{0},
#'     \mathbf{I}
#'     \right)
#'   }
#'   where
#'   \eqn{\mathbf{z}_{i, t}} is a vector of
#'   independent standard normal random variables and
#'   \eqn{
#'     \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right)
#'     \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right)^{\prime}
#'     =
#'     \boldsymbol{\Theta} .
#'   }
#'
#'   The dynamic structure is given by
#'   \deqn{
#'     \mathrm{d} \boldsymbol{\eta}_{i, t}
#'     =
#'     \left(
#'     \boldsymbol{\iota}
#'     +
#'     \boldsymbol{\Phi}
#'     \boldsymbol{\eta}_{i, t}
#'     \right)
#'     \mathrm{d}t
#'     +
#'     \boldsymbol{\Sigma}^{\frac{1}{2}}
#'     \mathrm{d}
#'     \mathbf{W}_{i, t}
#'   }
#'   where
#'   \eqn{\boldsymbol{\iota}}
#'   is a term which is unobserved and constant over time,
#'   \eqn{\boldsymbol{\Phi}}
#'   is the drift matrix
#'   which represents the rate of change of the solution
#'   in the absence of any random fluctuations,
#'   \eqn{\boldsymbol{\Sigma}}
#'   is the matrix of volatility
#'   or randomness in the process, and
#'   \eqn{\mathrm{d}\boldsymbol{W}}
#'   is a Wiener process or Brownian motion,
#'   which represents random fluctuations.
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
  output <- .ExpCov(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t
  )
  colnames(output) <- colnames(phi)
  rownames(output) <- rownames(phi)
  return(output)
}

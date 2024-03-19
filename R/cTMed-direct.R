#' Direct Effect of X on Y
#' Over a Specific Time-Interval
#'
#' This function computes the direct effect
#' of the independent variable \eqn{X}
#' on the dependent variable \eqn{Y}
#' through mediator variables \eqn{\mathbf{m}}
#' over a specific time-interval \eqn{\Delta t}
#' using the first-order stochastic differential equation model's
#' drift matrix \eqn{\boldsymbol{\Phi}}.
#'
#' @details The direct effect
#'   of the independent variable \eqn{X}
#'   on the dependent variable \eqn{Y}
#'   relative to some mediator variables \eqn{\mathbf{m}}
#'   is given by
#'   \deqn{
#'     \mathrm{Direct}_{{\Delta t}_{i, j, \mathrm{\mathbf{m}}}}
#'     =
#'     \exp
#'     \left(
#'       \Delta t
#'       \mathbf{D}
#'       \boldsymbol{\Phi}
#'       \mathbf{D}
#'     \right)_{i, j}
#'   }
#'   where
#'   \eqn{\boldsymbol{\Phi}} denotes the drift matrix,
#'   \eqn{\mathbf{D}} a diagonal matrix
#'   where the diagonal elements corresponding
#'   to mediator variables \eqn{\mathbf{m}}
#'   are set to zero and the rest to one,
#'   \eqn{i} the row index of \eqn{Y} in \eqn{\boldsymbol{\Phi}},
#'   \eqn{j} the column index of \eqn{X} in \eqn{\boldsymbol{\Phi}}, and
#'   \eqn{\Delta t} the time-interval.
#'
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
#' @inheritParams Indirect
#' @inherit Indirect references
#'
#' @return Returns an object
#'   of class `ctmedeffect` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("Direct").}
#'     \item{output}{The direct effect.}
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
#' delta_t <- 1
#' Direct(
#'   phi = phi,
#'   delta_t = delta_t,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' phi <- matrix(
#'   data = c(
#'     -6, 5.5, 0, 0,
#'     1.25, -2.5, 5.9, -7.3,
#'     0, 0, -6, 2.5,
#'     5, 0, 0, -6
#'   ),
#'   nrow = 4
#' )
#' colnames(phi) <- rownames(phi) <- paste0("y", 1:4)
#' Direct(
#'   phi = phi,
#'   delta_t = delta_t,
#'   from = "y2",
#'   to = "y4",
#'   med = c("y1", "y3")
#' )
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed effects
#' @export
Direct <- function(phi,
                   delta_t,
                   from,
                   to,
                   med) {
  idx <- rownames(phi)
  stopifnot(
    idx == colnames(phi),
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
    delta_t = delta_t,
    from = from,
    to = to,
    med = med
  )
  output <- .Direct(
    phi = phi,
    delta_t = delta_t,
    from = which(idx == from),
    to = which(idx == to),
    med = sapply(
      X = med,
      FUN = function(x,
                     idx) {
        return(
          which(idx == x)
        )
      },
      idx = idx
    )
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "Direct",
    output = output
  )
  class(out) <- c(
    "ctmedeffect",
    class(out)
  )
  return(out)
}

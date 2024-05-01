#' Delta Method Sampling Variance-Covariance Matrix
#' for the Elements of the Matrix of Lagged Coefficients
#' Over a Specific Time-Interval
#' or a Range of Time-Intervals
#'
#' This function computes the delta method
#' sampling variance-covariance matrix
#' for the elements of the matrix of lagged coefficients
#' \eqn{\boldsymbol{\beta}}
#' over a specific time-interval \eqn{\Delta t}
#' or a range of time-intervals
#' using the first-order stochastic differential equation model's
#' drift matrix \eqn{\boldsymbol{\Phi}}.
#'
#' @details See [Total()].
#'
#' ## Delta Method
#'   Let \eqn{\boldsymbol{\theta}} be
#'   \eqn{\mathrm{vec} \left( \boldsymbol{\Phi} \right)},
#'   that is,
#'   the elements of the \eqn{\boldsymbol{\Phi}} matrix
#'   in vector form sorted column-wise.
#'   Let \eqn{\hat{\boldsymbol{\theta}}} be
#'   \eqn{\mathrm{vec} \left( \hat{\boldsymbol{\Phi}} \right)}.
#'   By the multivariate central limit theory,
#'   the function \eqn{\mathbf{g}}
#'   using \eqn{\hat{\boldsymbol{\theta}}} as input
#'   can be expressed as:
#'
#'   \deqn{
#'   	\sqrt{n}
#'   	\left(
#'   	\mathbf{g} \left( \hat{\boldsymbol{\theta}} \right)
#'   	-
#'   	\mathbf{g} \left( \boldsymbol{\theta} \right)
#'   	\right)
#'   	\xrightarrow[]{
#'   		\mathrm{D}
#'   	}
#'   	\mathcal{N}
#'   	\left(
#'   	0,
#'   	\mathbf{J}
#'   	\boldsymbol{\Gamma}
#'   	\mathbf{J}^{\prime}
#'   	\right)
#'   }
#'
#'   where \eqn{\mathbf{J}} is the matrix of first-order derivatives
#'   of the function \eqn{\mathbf{g}}
#'   with respect to the elements of \eqn{\boldsymbol{\theta}}
#'   and
#'   \eqn{\boldsymbol{\Gamma}}
#'   is the asymptotic variance-covariance matrix of
#'   \eqn{\hat{\boldsymbol{\theta}}}.
#'
#'   From the former,
#'   we can derive the distribution of
#'   \eqn{\mathbf{g} \left( \hat{\boldsymbol{\theta}} \right)} as follows:
#'
#'   \deqn{
#'   	\mathbf{g} \left( \hat{\boldsymbol{\theta}} \right)
#'   	\approx
#'   	\mathcal{N}
#'   	\left(
#'   	\mathbf{g} \left( \boldsymbol{\theta} \right)
#'   	,
#'   	n^{-1}
#'   	\mathbf{J}
#'   	\boldsymbol{\Gamma}
#'   	\mathbf{J}^{\prime}
#'   	\right)
#'   }
#'
#'   The uncertainty associated with the estimator
#'   \eqn{\mathbf{g} \left( \hat{\boldsymbol{\theta}} \right)}
#'   is, therefore, given by
#'   \eqn{n^{-1} \mathbf{J} \boldsymbol{\Gamma} \mathbf{J}^{\prime}} .
#'   When \eqn{\boldsymbol{\Gamma}} is unknown,
#'   by substitution,
#'   we can use
#'   the estimated sampling variance-covariance matrix of
#'   \eqn{\hat{\boldsymbol{\theta}}},
#'   that is,
#'   \eqn{\hat{\mathbb{V}} \left( \hat{\boldsymbol{\theta}} \right)}
#'   for \eqn{n^{-1} \boldsymbol{\Gamma}}.
#'   Therefore,
#'   the sampling variance-covariance matrix of
#'   \eqn{\mathbf{g} \left( \hat{\boldsymbol{\theta}} \right)}
#'   is given by
#'
#'   \deqn{
#'   	\mathbf{g} \left( \hat{\boldsymbol{\theta}} \right)
#'   	\approx
#'   	\mathcal{N}
#'   	\left(
#'   	\mathbf{g} \left( \boldsymbol{\theta} \right)
#'   	,
#'   	\mathbf{J}
#'   	\hat{\mathbb{V}} \left( \hat{\boldsymbol{\theta}} \right)
#'   	\mathbf{J}^{\prime}
#'   	\right) .
#'   }
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
#' @param ncores Positive integer.
#'   Number of cores to use.
#'   If `ncores = NULL`,
#'   use a single core.
#'   Consider using multiple cores
#'   when the length of `delta_t` is long.
#' @inheritParams Med
#' @inheritParams MCMed
#' @inherit Indirect references
#'
#' @return Returns an object
#'   of class `ctmeddelta` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used (DeltaBeta).}
#'     \item{output}{A list with length of `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{delta_t}{Time-interval.}
#'     \item{jacobian}{Jacobian matrix.}
#'     \item{est}{Estimated total, direct, and indirect effects.}
#'     \item{vcov}{Sampling variance-covariance matrix of the
#'     estimated total, direct, and indirect effects.}
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
#' vcov_phi_vec <- matrix(
#'   data = c(
#'     0.002704274, -0.001475275, 0.000949122,
#'     -0.001619422, 0.000885122, -0.000569404,
#'     0.00085493, -0.000465824, 0.000297815,
#'     -0.001475275, 0.004428442, -0.002642303,
#'     0.000980573, -0.00271817, 0.001618805,
#'     -0.000586921, 0.001478421, -0.000871547,
#'     0.000949122, -0.002642303, 0.006402668,
#'     -0.000697798, 0.001813471, -0.004043138,
#'     0.000463086, -0.001120949, 0.002271711,
#'     -0.001619422, 0.000980573, -0.000697798,
#'     0.002079286, -0.001152501, 0.000753,
#'     -0.001528701, 0.000820587, -0.000517524,
#'     0.000885122, -0.00271817, 0.001813471,
#'     -0.001152501, 0.00342605, -0.002075005,
#'     0.000899165, -0.002532849, 0.001475579,
#'     -0.000569404, 0.001618805, -0.004043138,
#'     0.000753, -0.002075005, 0.004984032,
#'     -0.000622255, 0.001634917, -0.003705661,
#'     0.00085493, -0.000586921, 0.000463086,
#'     -0.001528701, 0.000899165, -0.000622255,
#'     0.002060076, -0.001096684, 0.000686386,
#'     -0.000465824, 0.001478421, -0.001120949,
#'     0.000820587, -0.002532849, 0.001634917,
#'     -0.001096684, 0.003328692, -0.001926088,
#'     0.000297815, -0.000871547, 0.002271711,
#'     -0.000517524, 0.001475579, -0.003705661,
#'     0.000686386, -0.001926088, 0.004726235
#'   ),
#'   nrow = 9
#' )
#'
#' # Specific time-interval ----------------------------------------------------
#' DeltaBeta(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1
#' )
#'
#' # Range of time-intervals ---------------------------------------------------
#' delta <- DeltaBeta(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5
#' )
#' plot(delta)
#'
#' # Methods -------------------------------------------------------------------
#' # DeltaBeta has a number of methods including
#' # print, summary, confint, and plot
#' print(delta)
#' summary(delta)
#' confint(delta, level = 0.95)
#' plot(delta)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed uncertainty beta
#' @export
DeltaBeta <- function(phi,
                      vcov_phi_vec,
                      delta_t,
                      ncores = NULL) {
  idx <- rownames(phi)
  stopifnot(
    idx == colnames(phi)
  )
  args <- list(
    phi = phi,
    vcov_phi_vec = vcov_phi_vec,
    delta_t = delta_t,
    ncores = ncores,
    method = "delta",
    network = FALSE
  )
  delta_t <- sort(
    ifelse(
      test = delta_t <= 0,
      yes = .Machine$double.xmin,
      no = delta_t
    )
  )
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    output <- parallel::parLapply(
      cl = cl,
      X = delta_t,
      fun = .DeltaBeta,
      phi = phi,
      vcov_phi_vec = vcov_phi_vec
    )
  } else {
    output <- lapply(
      X = delta_t,
      FUN = .DeltaBeta,
      phi = phi,
      vcov_phi_vec = vcov_phi_vec
    )
  }
  names(output) <- delta_t
  out <- list(
    call = match.call(),
    args = args,
    fun = "DeltaBeta",
    output = output
  )
  class(out) <- c(
    "ctmeddelta",
    class(out)
  )
  return(out)
}

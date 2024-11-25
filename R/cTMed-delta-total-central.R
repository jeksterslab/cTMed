#' Delta Method Sampling Variance-Covariance Matrix
#' for the Total Effect Centrality
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function computes the delta method
#' sampling variance-covariance matrix
#' for the total effect centrality
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
#' using the first-order stochastic differential equation model's
#' drift matrix \eqn{\boldsymbol{\Phi}}.
#'
#' @details See [TotalCentral()] more details.
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
#'     \item{fun}{Function used ("DeltaTotalCentral").}
#'     \item{output}{A list the length of which is equal to
#'         the length of `delta_t`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{delta_t}{Time interval.}
#'     \item{jacobian}{Jacobian matrix.}
#'     \item{est}{Estimated total effect centrality.}
#'     \item{vcov}{Sampling variance-covariance matrix of
#'        estimated total effect centrality.}
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
#'     0.00843, 0.00040, -0.00151,
#'     -0.00600, -0.00033, 0.00110,
#'     0.00324, 0.00020, -0.00061,
#'     0.00040, 0.00374, 0.00016,
#'     -0.00022, -0.00273, -0.00016,
#'     0.00009, 0.00150, 0.00012,
#'     -0.00151, 0.00016, 0.00389,
#'     0.00103, -0.00007, -0.00283,
#'     -0.00050, 0.00000, 0.00156,
#'     -0.00600, -0.00022, 0.00103,
#'     0.00644, 0.00031, -0.00119,
#'     -0.00374, -0.00021, 0.00070,
#'     -0.00033, -0.00273, -0.00007,
#'     0.00031, 0.00287, 0.00013,
#'     -0.00014, -0.00170, -0.00012,
#'     0.00110, -0.00016, -0.00283,
#'     -0.00119, 0.00013, 0.00297,
#'     0.00063, -0.00004, -0.00177,
#'     0.00324, 0.00009, -0.00050,
#'     -0.00374, -0.00014, 0.00063,
#'     0.00495, 0.00024, -0.00093,
#'     0.00020, 0.00150, 0.00000,
#'     -0.00021, -0.00170, -0.00004,
#'     0.00024, 0.00214, 0.00012,
#'     -0.00061, 0.00012, 0.00156,
#'     0.00070, -0.00012, -0.00177,
#'     -0.00093, 0.00012, 0.00223
#'   ),
#'   nrow = 9
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' DeltaTotalCentral(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' delta <- DeltaTotalCentral(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5
#' )
#' plot(delta)
#'
#' # Methods -------------------------------------------------------------------
#' # DeltaTotalCentral has a number of methods including
#' # print, summary, confint, and plot
#' print(delta)
#' summary(delta)
#' confint(delta, level = 0.95)
#' plot(delta)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed network delta
#' @export
DeltaTotalCentral <- function(phi,
                              vcov_phi_vec,
                              delta_t,
                              ncores = NULL,
                              tol = 0.01) {
  total <- TRUE
  args <- list(
    phi = phi,
    vcov_phi_vec = vcov_phi_vec,
    delta_t = delta_t,
    ncores = ncores,
    method = "delta",
    network = TRUE,
    total = total
  )
  delta_t <- sort(
    ifelse(
      test = delta_t < tol,
      yes = tol, # .Machine$double.xmin
      no = delta_t
    )
  )
  # nocov start
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
      fun = .DeltaCentral,
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      total = total
    )
    # nocov end
  } else {
    output <- lapply(
      X = delta_t,
      FUN = .DeltaCentral,
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      total = total
    )
  }
  names(output) <- delta_t
  out <- list(
    call = match.call(),
    args = args,
    fun = "DeltaTotalCentral",
    output = output
  )
  class(out) <- c(
    "ctmeddelta",
    class(out)
  )
  return(out)
}

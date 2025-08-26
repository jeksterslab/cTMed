#' Delta Method Sampling Variance-Covariance Matrix
#' for the Standardized Total, Direct, and Indirect Effects of X on Y
#' Through M
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function computes the delta method
#' sampling variance-covariance matrix
#' for the standardized total, direct, and indirect effects
#' of the independent variable \eqn{X}
#' on the dependent variable \eqn{Y}
#' through mediator variables \eqn{\mathbf{m}}
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
#' using the first-order stochastic differential equation model's
#' drift matrix \eqn{\boldsymbol{\Phi}}
#' and process noise covariance matrix \eqn{\boldsymbol{\Sigma}}.
#'
#' @details See [TotalStd()],
#'   [DirectStd()], and
#'   [IndirectStd()] for more details.
#'
#' ## Delta Method
#'   Let \eqn{\boldsymbol{\theta}} be
#'   a vector that combines
#'   \eqn{\mathrm{vec} \left( \boldsymbol{\Phi} \right)},
#'   that is,
#'   the elements of the \eqn{\boldsymbol{\Phi}} matrix
#'   in vector form sorted column-wise and
#'   \eqn{\mathrm{vech} \left( \boldsymbol{\Sigma} \right)},
#'   that is,
#'   the unique elements of the \eqn{\boldsymbol{\Sigma}} matrix
#'   in vector form sorted column-wise.
#'   Let \eqn{\hat{\boldsymbol{\theta}}} be
#'   a vector that combines
#'   \eqn{\mathrm{vec} \left( \hat{\boldsymbol{\Phi}} \right)} and
#'   \eqn{\mathrm{vech} \left( \hat{\boldsymbol{\Sigma}} \right)}.
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
#' @inheritParams MCMedStd
#' @inherit IndirectStd references
#'
#' @return Returns an object
#'   of class `ctmeddelta` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("DeltaMedStd").}
#'     \item{output}{A list the length of which is equal to
#'         the length of `delta_t`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{delta_t}{Time interval.}
#'     \item{jacobian}{Jacobian matrix.}
#'     \item{est}{Estimated standardized total, direct, and indirect effects.}
#'     \item{vcov}{Sampling variance-covariance matrix of the
#'     estimated standardized total, direct, and indirect effects.}
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
#' vcov_theta <- matrix(
#'   data = c(
#'     0.00843, 0.00040, -0.00151, -0.00600, -0.00033,
#'     0.00110, 0.00324, 0.00020, -0.00061, -0.00115,
#'     0.00011, 0.00015, 0.00001, -0.00002, -0.00001,
#'     0.00040, 0.00374, 0.00016, -0.00022, -0.00273,
#'     -0.00016, 0.00009, 0.00150, 0.00012, -0.00010,
#'     -0.00026, 0.00002, 0.00012, 0.00004, -0.00001,
#'     -0.00151, 0.00016, 0.00389, 0.00103, -0.00007,
#'     -0.00283, -0.00050, 0.00000, 0.00156, 0.00021,
#'     -0.00005, -0.00031, 0.00001, 0.00007, 0.00006,
#'     -0.00600, -0.00022, 0.00103, 0.00644, 0.00031,
#'     -0.00119, -0.00374, -0.00021, 0.00070, 0.00064,
#'     -0.00015, -0.00005, 0.00000, 0.00003, -0.00001,
#'     -0.00033, -0.00273, -0.00007, 0.00031, 0.00287,
#'     0.00013, -0.00014, -0.00170, -0.00012, 0.00006,
#'     0.00014, -0.00001, -0.00015, 0.00000, 0.00001,
#'     0.00110, -0.00016, -0.00283, -0.00119, 0.00013,
#'     0.00297, 0.00063, -0.00004, -0.00177, -0.00013,
#'     0.00005, 0.00017, -0.00002, -0.00008, 0.00001,
#'     0.00324, 0.00009, -0.00050, -0.00374, -0.00014,
#'     0.00063, 0.00495, 0.00024, -0.00093, -0.00020,
#'     0.00006, -0.00010, 0.00000, -0.00001, 0.00004,
#'     0.00020, 0.00150, 0.00000, -0.00021, -0.00170,
#'     -0.00004, 0.00024, 0.00214, 0.00012, -0.00002,
#'     -0.00004, 0.00000, 0.00006, -0.00005, -0.00001,
#'     -0.00061, 0.00012, 0.00156, 0.00070, -0.00012,
#'     -0.00177, -0.00093, 0.00012, 0.00223, 0.00004,
#'     -0.00002, -0.00003, 0.00001, 0.00003, -0.00013,
#'     -0.00115, -0.00010, 0.00021, 0.00064, 0.00006,
#'     -0.00013, -0.00020, -0.00002, 0.00004, 0.00057,
#'     0.00001, -0.00009, 0.00000, 0.00000, 0.00001,
#'     0.00011, -0.00026, -0.00005, -0.00015, 0.00014,
#'     0.00005, 0.00006, -0.00004, -0.00002, 0.00001,
#'     0.00012, 0.00001, 0.00000, -0.00002, 0.00000,
#'     0.00015, 0.00002, -0.00031, -0.00005, -0.00001,
#'     0.00017, -0.00010, 0.00000, -0.00003, -0.00009,
#'     0.00001, 0.00014, 0.00000, 0.00000, -0.00005,
#'     0.00001, 0.00012, 0.00001, 0.00000, -0.00015,
#'     -0.00002, 0.00000, 0.00006, 0.00001, 0.00000,
#'     0.00000, 0.00000, 0.00010, 0.00001, 0.00000,
#'     -0.00002, 0.00004, 0.00007, 0.00003, 0.00000,
#'     -0.00008, -0.00001, -0.00005, 0.00003, 0.00000,
#'     -0.00002, 0.00000, 0.00001, 0.00005, 0.00001,
#'     -0.00001, -0.00001, 0.00006, -0.00001, 0.00001,
#'     0.00001, 0.00004, -0.00001, -0.00013, 0.00001,
#'     0.00000, -0.00005, 0.00000, 0.00001, 0.00012
#'   ),
#'   nrow = 15
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' DeltaMedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   vcov_theta = vcov_theta,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' delta <- DeltaMedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   vcov_theta = vcov_theta,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(delta)
#'
#' # Methods -------------------------------------------------------------------
#' # DeltaMedStd has a number of methods including
#' # print, summary, confint, and plot
#' print(delta)
#' summary(delta)
#' confint(delta, level = 0.95)
#' plot(delta)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed path delta
#' @export
DeltaMedStd <- function(phi,
                        sigma,
                        vcov_theta,
                        delta_t,
                        from,
                        to,
                        med,
                        ncores = NULL,
                        tol = 0.01) {
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
    sigma = sigma,
    vcov_sigma_vech = vcov_theta,
    delta_t = delta_t,
    from = from,
    to = to,
    med = med,
    ncores = ncores,
    method = "delta",
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
  # nocov start
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    R <- length(delta_t)
    if (ncores > R) {
      ncores <- R
    }
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    os_type <- Sys.info()["sysname"]
    if (os_type == "Darwin") {
      fork <- TRUE
    } else if (os_type == "Linux") {
      fork <- TRUE
    } else {
      fork <- FALSE
    }
    if (fork) {
      output <- parallel::mclapply(
        X = delta_t,
        FUN = .DeltaMedStd,
        phi = phi,
        sigma = sigma,
        vcov_theta = vcov_theta,
        from = from,
        to = to,
        med = med,
        mc.cores = ncores
      )
    } else {
      cl <- parallel::makeCluster(ncores)
      on.exit(
        parallel::stopCluster(cl = cl)
      )
      output <- parallel::parLapply(
        cl = cl,
        X = delta_t,
        fun = .DeltaMedStd,
        phi = phi,
        sigma = sigma,
        vcov_theta = vcov_theta,
        from = from,
        to = to,
        med = med
      )
    }
    # nocov end
  } else {
    output <- lapply(
      X = delta_t,
      FUN = .DeltaMedStd,
      phi = phi,
      sigma = sigma,
      vcov_theta = vcov_theta,
      from = from,
      to = to,
      med = med
    )
  }
  names(output) <- delta_t
  out <- list(
    call = match.call(),
    args = args,
    fun = "DeltaMedStd",
    output = output
  )
  class(out) <- c(
    "ctmeddelta",
    class(out)
  )
  out
}

#' Generate Random Drift Matrices
#' Using the Monte Carlo Method
#'
#' This function generates random
#' drift matrices \eqn{\boldsymbol{\Phi}}
#' using the Monte Carlo method.
#'
#' @details
#' ## Monte Carlo Method
#'   Let \eqn{\boldsymbol{\theta}} be
#'   \eqn{\mathrm{vec} \left( \boldsymbol{\Phi} \right)},
#'   that is,
#'   the elements of the \eqn{\boldsymbol{\Phi}} matrix
#'   in vector form sorted column-wise.
#'   Let \eqn{\hat{\boldsymbol{\theta}}} be
#'   \eqn{\mathrm{vec} \left( \hat{\boldsymbol{\Phi}} \right)}.
#'   Based on the asymptotic properties of maximum likelihood estimators,
#'   we can assume that estimators are normally distributed
#'   around the population parameters.
#'   \deqn{
#'   	\hat{\boldsymbol{\theta}}
#'   	\sim
#'   	\mathcal{N}
#'   	\left(
#'   	\boldsymbol{\theta},
#'   	\mathbb{V} \left( \hat{\boldsymbol{\theta}} \right)
#'   	\right)
#'   }
#'   Using this distributional assumption,
#'   a sampling distribution of \eqn{\hat{\boldsymbol{\theta}}}
#'   which we refer to as \eqn{\hat{\boldsymbol{\theta}}^{\ast}}
#'   can be generated by replacing the population parameters
#'   with sample estimates,
#'   that is,
#'   \deqn{
#'   	\hat{\boldsymbol{\theta}}^{\ast}
#'   	\sim
#'   	\mathcal{N}
#'   	\left(
#'   	\hat{\boldsymbol{\theta}},
#'   	\hat{\mathbb{V}} \left( \hat{\boldsymbol{\theta}} \right)
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
#' @inheritParams Indirect
#' @param vcov_phi_vec Numeric matrix.
#'   The sampling variance-covariance matrix of
#'   \eqn{\mathrm{vec} \left( \boldsymbol{\Phi} \right)}.
#' @param R Positive integer.
#'   Number of replications.
#' @param test_phi Logical.
#'   If `test_phi = TRUE`,
#'   the function tests the stability
#'   of the generated drift matrix \eqn{\boldsymbol{\Phi}}.
#'   If the test returns `FALSE`,
#'   the function generates a new drift matrix \eqn{\boldsymbol{\Phi}}
#'   and runs the test recursively
#'   until the test returns `TRUE`.
#' @param ncores Positive integer.
#'   Number of cores to use.
#'   If `ncores = NULL`,
#'   use a single core.
#'   Consider using multiple cores
#'   when number of replications `R`
#'   is a large value.
#' @param seed Random seed.
#'
#' @return Returns an object
#'   of class `ctmedmc` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("MCPhi").}
#'     \item{output}{A list simulated drift matrices.}
#'   }
#'
#' @examples
#' set.seed(42)
#' phi <- matrix(
#'   data = c(
#'     -0.357, 0.771, -0.450,
#'     0.0, -0.511, 0.729,
#'     0, 0, -0.693
#'   ),
#'   nrow = 3
#' )
#' colnames(phi) <- rownames(phi) <- c("x", "m", "y")
#' MCPhi(
#'   phi = phi,
#'   vcov_phi_vec = 0.1 * diag(9),
#'   R = 100L # use a large value for R in actual research
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
#' MCPhi(
#'   phi = phi,
#'   vcov_phi_vec = 0.1 * diag(16),
#'   R = 100L, # use a large value for R in actual research
#'   test_phi = FALSE
#' )
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed uncertainty
#' @export
MCPhi <- function(phi,
                  vcov_phi_vec,
                  R,
                  test_phi = TRUE,
                  ncores = NULL,
                  seed = NULL) {
  idx <- rownames(phi)
  stopifnot(
    idx == colnames(phi)
  )
  args <- list(
    phi = phi,
    vcov_phi_vec = vcov_phi_vec,
    R = R,
    test_phi = test_phi,
    ncores = ncores,
    seed = seed,
    method = "mc"
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
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(
        cl = cl,
        iseed = seed
      )
    }
    output <- parallel::parLapply(
      cl = cl,
      X = 1:R,
      fun = function(i) {
        return(
          .MCPhiI(
            phi = phi,
            vcov_phi_vec_l = t(chol(vcov_phi_vec)),
            test_phi = test_phi
          )
        )
      }
    )
    # nocov end
  } else {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    output <- .MCPhi(
      phi = phi,
      vcov_phi_vec_l = t(chol(vcov_phi_vec)),
      R = R,
      test_phi = test_phi
    )
  }
  output <- lapply(
    X = output,
    FUN = function(x) {
      colnames(x) <- rownames(x) <- idx
      return(x)
    }
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "MCPhi",
    output = output
  )
  class(out) <- c(
    "ctmedmcphi",
    class(out)
  )
  return(out)
}

#' Posterior Sampling Distribution
#' for the Elements of the Matrix of Lagged Coefficients
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function generates a posterior
#' sampling distribution
#' for the elements of the matrix of lagged coefficients
#' \eqn{\boldsymbol{\beta}}
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
#' using the first-order stochastic differential equation model
#' drift matrix \eqn{\boldsymbol{\Phi}}.
#'
#' @details See [Total()].
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams Indirect
#' @inheritParams MCPhi
#' @inheritParams MCMed
#' @inherit Indirect references
#'
#' @return Returns an object
#'   of class `ctmedmc` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("PosteriorBeta").}
#'     \item{output}{A list the length of which is equal to
#'         the length of `delta_t`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{est}{A vector of total, direct, and indirect effects.}
#'     \item{thetahatstar}{A matrix of Monte Carlo
#'     total, direct, and indirect effects.}
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
#' phi <- MCPhi(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   R = 1000L
#' )$output
#'
#' # Specific time interval ----------------------------------------------------
#' PosteriorBeta(
#'   phi = phi,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' posterior <- PosteriorBeta(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' plot(posterior)
#'
#' # Methods -------------------------------------------------------------------
#' # PosteriorBeta has a number of methods including
#' # print, summary, confint, and plot
#' print(posterior)
#' summary(posterior)
#' confint(posterior, level = 0.95)
#' plot(posterior)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed beta posterior
#' @export
PosteriorBeta <- function(phi,
                          delta_t,
                          ncores = NULL,
                          tol = 0.01) {
  idx <- rownames(phi)
  stopifnot(
    idx == colnames(phi)
  )
  args <- list(
    phi = phi,
    delta_t = delta_t,
    ncores = ncores,
    method = "posterior",
    network = FALSE
  )
  delta_t <- sort(
    ifelse(
      test = delta_t < tol,
      yes = tol, # .Machine$double.xmin
      no = delta_t
    )
  )
  output <- .PosteriorBeta(
    phi = phi,
    delta_t = delta_t,
    ncores = ncores
  )
  names(output) <- delta_t
  out <- list(
    call = match.call(),
    args = args,
    fun = "PosteriorBeta",
    output = output
  )
  class(out) <- c(
    "ctmedmc",
    class(out)
  )
  return(out)
}

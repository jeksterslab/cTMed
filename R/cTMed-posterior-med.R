#' Posterior Distribution
#' of Total, Direct, and Indirect Effects
#' of X on Y Through M
#' Over a Specific Time-Interval
#'
#' This function generates a posterior
#' distribution
#' of the total, direct and indirect effects
#' of the independent variable \eqn{X}
#' on the dependent variable \eqn{Y}
#' through mediator variables \eqn{\mathbf{m}}
#' at a particular time-interval \eqn{\Delta t}
#' using the posterior distribution
#' of the first-order stochastic differential equation model
#' drift matrix \eqn{\boldsymbol{\Phi}}.
#'
#' @details See [Total()],
#'   [Direct()], and
#'   [Indirect()] for more details.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams MCMed
#' @param phi List of numeric matrices.
#'   Each element of the list is a sample
#'   from the posterior distribution
#'   of the drift matrix (\eqn{\boldsymbol{\Phi}}).
#'   Each matrix should have row and column names
#'   pertaining to the variables in the system.
#'
#' @inherit Indirect references
#'
#' @return Returns an object
#'   of class `ctmedmc` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used (PosteriorMed).}
#'     \item{output}{A list with length of `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{est}{Mean of the posterior distribution
#'     of the total, direct, and indirect effects.}
#'     \item{thetahatstar}{Posterior distribution of the
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
#' phi <- MCPhi(
#'   phi = phi,
#'   vcov_phi_vec = 0.1 * diag(9),
#'   R = 100L # use a large value for R in actual research
#' )$output
#'
#' # Specific time-interval ----------------------------------------------------
#' PosteriorMed(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#'
#' # Range of time-intervals ---------------------------------------------------
#' posterior <- PosteriorMed(
#'   phi = phi,
#'   delta_t = 1:30,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#'
#' # Methods -------------------------------------------------------------------
#' # PosteriorMed has a number of methods including
#' # print, summary, confint, and plot
#' print(posterior)
#' summary(posterior)
#' confint(posterior, level = 0.95)
#' plot(posterior)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed uncertainty
#' @export
PosteriorMed <- function(phi,
                         delta_t,
                         from,
                         to,
                         med,
                         ncores = NULL) {
  stopifnot(
    is.list(phi),
    is.matrix(phi[[1]])
  )
  idx <- rownames(phi[[1]])
  stopifnot(
    idx == colnames(phi[[1]]),
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
    med = med,
    ncores = ncores
  )
  delta_t <- sort(
    ifelse(
      test = delta_t <= 0,
      yes = .Machine$double.xmin,
      no = delta_t
    )
  )
  from <- which(idx == from)
  to <- which(idx == to)
  med <- sapply(
    X = med,
    FUN = function(x,
                   idx) {
      return(
        which(idx == x)
      )
    },
    idx = idx
  )
  output <- .PosteriorMed(
    phi = phi,
    delta_t = delta_t,
    from = from,
    to = to,
    med = med,
    ncores = ncores
  )
  names(output) <- delta_t
  out <- list(
    call = match.call(),
    args = args,
    fun = "PosteriorMed",
    output = output
  )
  class(out) <- c(
    "ctmedmc",
    class(out)
  )
  return(out)
}

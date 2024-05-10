#' Posterior Distribution
#' of Total, Direct, and Indirect Effects
#' of X on Y Through M
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function generates a posterior
#' distribution
#' of the total, direct and indirect effects
#' of the independent variable \eqn{X}
#' on the dependent variable \eqn{Y}
#' through mediator variables \eqn{\mathbf{m}}
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
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
#' phi <- MCPhi(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   R = 1000L
#' )$output
#'
#' # Specific time interval ----------------------------------------------------
#' PosteriorMed(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' posterior <- PosteriorMed(
#'   phi = phi,
#'   delta_t = 1:5,
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
#' @keywords cTMed uncertainty path
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
    ncores = ncores,
    method = "posterior",
    network = FALSE
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

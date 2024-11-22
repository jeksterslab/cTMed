#' Standardized Total, Direct, and Indirect Effects of X on Y
#' Through M
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function computes the standardized total, direct, and indirect effects
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
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams IndirectStd
#' @inherit IndirectStd references
#' @param delta_t Vector of positive numbers.
#'   Time interval
#'   (\eqn{\Delta t}).
#'
#' @return Returns an object
#'   of class `ctmedmed` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("MedStd").}
#'     \item{output}{A matrix of total, direct, and indirect effects.}
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
#'     0.24, 0.02, -0.05,
#'     0.02, 0.07, 0.02,
#'     -0.05, 0.02, 0.08
#'   ),
#'   nrow = 3
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' MedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' med <- MedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:30,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(med)
#'
#' # Methods -------------------------------------------------------------------
#' # MedStd has a number of methods including
#' # print, summary, and plot
#' med <- MedStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(med)
#' summary(med)
#' plot(med)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed effects path
#' @export
MedStd <- function(phi,
                   sigma,
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
  delta_t <- ifelse(
    test = delta_t <= 0,
    yes = .Machine$double.xmin,
    no = delta_t
  )
  args <- list(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t,
    from = from,
    to = to,
    med = med,
    network = FALSE
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
  if (length(delta_t) > 1) {
    output <- .MedStds(
      phi = phi,
      sigma = sigma,
      delta_t = delta_t,
      from = from,
      to = to,
      med = med
    )
    colnames(output) <- c(
      "total",
      "direct",
      "indirect",
      "interval"
    )
  } else {
    output <- matrix(
      data = .MedStd(
        phi = phi,
        sigma = sigma,
        delta_t = delta_t,
        from = from,
        to = to,
        med = med
      ),
      nrow = 1
    )
    colnames(output) <- c(
      "total",
      "direct",
      "indirect",
      "interval"
    )
  }
  out <- list(
    call = match.call(),
    args = args,
    fun = "MedStd",
    output = output
  )
  class(out) <- c(
    "ctmedmed",
    class(out)
  )
  return(out)
}

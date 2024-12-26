#' Bootstrap Sampling Distribution
#' of Total, Direct, and Indirect Effects
#' of X on Y Through M
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function generates a bootstrap method
#' sampling distribution
#' of the total, direct and indirect effects
#' of the independent variable \eqn{X}
#' on the dependent variable \eqn{Y}
#' through mediator variables \eqn{\mathbf{m}}
#' over a specific time interval \eqn{\Delta t}
#' or a range of time intervals
#' using the first-order stochastic differential equation model
#' drift matrix \eqn{\boldsymbol{\Phi}}.
#'
#' @details See [Total()],
#'   [Direct()], and
#'   [Indirect()] for more details.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param phi List of numeric matrices.
#'   Each element of the list is a bootstrap estimate
#'   of the drift matrix (\eqn{\boldsymbol{\Phi}}).
#'   Each matrix should have row and column names
#'   pertaining to the variables in the system.
#' @param phi_hat Numeric matrix.
#'   The estimated drift matrix (\eqn{\hat{\boldsymbol{\Phi}}})
#'   from the original data set.
#'   `phi_hat` should have row and column names
#'   pertaining to the variables in the system.
#' @inheritParams Indirect
#' @inheritParams MCPhi
#' @inheritParams Med
#' @inherit Indirect references
#'
#' @return Returns an object
#'   of class `ctmedmc` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("BootMed").}
#'     \item{output}{A list with length of `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{est}{A vector of total, direct, and indirect effects.}
#'     \item{thetahatstar}{A matrix of bootstrap
#'     total, direct, and indirect effects.}
#'   }
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed path boot
#' @export
BootMed <- function(phi,
                    phi_hat,
                    delta_t,
                    from,
                    to,
                    med,
                    ncores = NULL,
                    tol = 0.01) {
  idx <- rownames(phi_hat)
  stopifnot(
    idx == colnames(phi_hat),
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
    phi_hat = phi_hat,
    delta_t = delta_t,
    from = from,
    to = to,
    med = med,
    ncores = ncores,
    method = "boot",
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
      return(
        which(idx == x)
      )
    },
    idx = idx
  )
  output <- .BootMed(
    phi = phi,
    phi_hat = phi_hat,
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
    fun = "BootMed",
    output = output
  )
  class(out) <- c(
    "ctmedmc",
    class(out)
  )
  return(out)
}

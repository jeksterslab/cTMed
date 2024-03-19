#' Plot Results of The MCMed or the PosteriorMed Functions
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [MCMed()] or the [PosteriorMed()] functions.
#' @param alpha Numeric.
#'   Significance level.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Specific time-interval ----------------------------------------------------
#' MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#'
#' # Range of time-intervals ---------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' plot(mc)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotMCMed <- function(object,
                       alpha = 0.05) {
  if (length(object$output) == 1) {
    stop(
      paste0(
        "The input argument \'object\' only has a single `delta_t` value.",
        "\n",
        "Not suitable for plotting.",
        "\n"
      )
    )
  }
  stopifnot(length(alpha) == 1)
  stopifnot(
    alpha > 0 && alpha < 1
  )
  if (object$fun == "PosteriorMed") {
    ylab <- "Posterior"
    method <- "Posterior"
  }
  if (object$fun == "MCMed") {
    ylab <- "Estimate"
    method <- "Monte Carlo Method"
  }
  ci <- .MCCI(
    object = object,
    alpha = alpha
  )
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  colnames(ci) <- c(
    "interval",
    "est",
    "se",
    "R",
    "ll",
    "ul"
  )
  effect <- rownames(ci)
  ci <- as.data.frame(
    ci
  )
  ci$effect <- effect
  rownames(ci) <- NULL
  foo <- function(effect,
                  ci) {
    if (effect == "indirect") {
      col <- "#e41a1c"
    }
    if (effect == "direct") {
      col <- "#377eb8"
    }
    if (effect == "total") {
      col <- "#4daf4a"
    }
    ci <- ci[which(ci$effect == effect), ]
    graphics::plot.default(
      x = 0,
      y = 0,
      xlim = range(ci$interval),
      ylim = range(c(ci$est, ci$ll, ci$ul)),
      type = "n",
      xlab = "Time-Interval",
      ylab = ylab,
      main = paste0(
        (1 - alpha) * 100,
        "% CI for the ",
        gsub(
          pattern = "(^|[[:space:]])([[:alpha:]])",
          replacement = "\\1\\U\\2",
          x = effect,
          perl = TRUE
        ),
        " Effect (",
        method,
        ")"
      )
    )
    graphics::abline(
      h = 0
    )
    graphics::lines(
      x = ci$interval,
      y = ci$est,
      type = "l",
      col = col,
      lty = 1,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ll,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ul,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
  }
  foo(effect = "total", ci = ci)
  foo(effect = "direct", ci = ci)
  foo(effect = "indirect", ci = ci)
}

#' Plot Results of The Med Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [Med()] function.
#' @param col_direct Character string.
#'   Optional argument.
#'   Color for the direct effect.
#' @param col_indirect Character string.
#'   Optional argument.
#'   Color for the indirect effect.
#' @param col_total Character string.
#'   Optional argument.
#'   Color for the total effect.
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
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:30,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(med)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotMed <- function(object,
                     col_direct = "#2c7bb6",
                     col_indirect = "#d7191c",
                     col_total = "#5e3c99") {
  if (dim(object$output)[1] == 1) {
    stop(
      paste0(
        "The input argument \'object\' only has a single `delta_t` value.",
        "\n",
        "Not suitable for plotting.",
        "\n"
      )
    )
  }
  delta_t <- object$output[, "interval"]
  graphics::plot.default(
    x = 0,
    y = 0,
    xlim = range(delta_t),
    ylim = range(
      object$output[
        ,
        c(
          "total",
          "direct",
          "indirect"
        )
      ]
    ),
    type = "n",
    xlab = "Time-Interval",
    ylab = "Parameter Value",
    main = "Total, Direct, and Indirect Effects"
  )
  graphics::abline(
    h = 0
  )
  graphics::lines(
    x = delta_t,
    y = object$output[
      ,
      "indirect"
    ],
    type = "l",
    col = col_indirect,
    lty = 1,
    lwd = 2
  )
  graphics::lines(
    x = delta_t,
    y = object$output[
      ,
      "direct"
    ],
    type = "l",
    col = col_direct,
    lty = 2,
    lwd = 2
  )
  graphics::lines(
    x = delta_t,
    y = object$output[
      ,
      "total"
    ],
    type = "l",
    col = col_total,
    lty = 3,
    lwd = 2
  )
  graphics::legend(
    x = "topright",
    legend = c("Indirect", "Direct", "Total"),
    lty = c(1, 2, 3),
    col = c(col_indirect, col_direct, col_total),
    cex = 0.8,
    lwd = 2
  )
}

#' Plot Results of The Med Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [Med()] function.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Example 1 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
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
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(med)
#'
#' # ---------------------------------------------------------------------------
#' # Example 2 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
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
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:10,
#'   from = "y2",
#'   to = "y4",
#'   med = c("y1", "y3")
#' )
#' plot(med)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotMed <- function(object) {
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
    col = "#e41a1c",
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
    col = "#377eb8",
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
    col = "#4daf4a",
    lty = 3,
    lwd = 2
  )
  graphics::legend(
    x = "topright",
    legend = c("Indirect", "Direct", "Total"),
    lty = c(1, 2, 3),
    col = c("#e41a1c", "#377eb8", "#4daf4a"),
    cex = 0.8,
    lwd = 2
  )
}

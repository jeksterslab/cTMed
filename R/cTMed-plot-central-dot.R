#' Plot Results of The TotalCentral
#' or The IndirectCentral Functions
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [TotalCentral()] or the [IndirectCentral()] functions.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
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
#' # Range of time intervals ---------------------------------------------------
#' total_central <- TotalCentral(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' plot(total_central)
#' indirect_central <- IndirectCentral(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' plot(indirect_central)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotCentral <- function(object,
                         col = NULL,
                         legend_pos = "topright") {
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
  if (object$args$total) {
    main <- "Total Effect Centrality"
  } else {
    main <- "Indirect Effect Centrality"
  }
  delta_t <- object$output[, "interval"]
  varnames <- colnames(
    object$args$phi
  )
  if (is.null(col)) {
    col <- grDevices::rainbow(length(varnames))
  }
  graphics::plot.default(
    x = 0,
    y = 0,
    xlim = range(delta_t),
    ylim = range(
      object$output[
        ,
        varnames
      ]
    ),
    type = "n",
    xlab = "Time Interval",
    ylab = "Parameter Value",
    main = main
  )
  graphics::abline(
    h = 0
  )
  for (i in seq_along(varnames)) {
    graphics::lines(
      x = delta_t,
      y = object$output[
        ,
        varnames[i]
      ],
      type = "l",
      col = col[i],
      lty = i,
      lwd = 2
    )
  }
  graphics::legend(
    x = legend_pos,
    legend = varnames,
    lty = seq_len(length(varnames)),
    col = col,
    cex = 0.8,
    lwd = 2
  )
  invisible(NULL)
}

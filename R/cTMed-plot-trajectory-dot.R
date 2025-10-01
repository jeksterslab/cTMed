#' Plot Results of The Trajectory Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [Med()] function.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
#' @param total Logical.
#'   If `total = TRUE`, include the total effect trajectory.
#'   If `total = FALSE`, exclude the total effect trajectory.
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
#' traj <- Trajectory(
#'   mu0 = c(3, 3, -3),
#'   time = 150,
#'   phi = phi,
#'   med = "m"
#' )
#'
#' plot(traj)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotTrajectory <- function(object,
                            legend_pos = "topright",
                            total) {
  idx <- rownames(object$args$phi)
  p <- length(idx)
  ylab <- idx
  varnames <- paste0("y", seq_len(p))
  phi <- simStateSpace:::as.data.frame.simstatespace(
    object$output$total
  )
  phi_direct <- simStateSpace:::as.data.frame.simstatespace(
    object$output$direct
  )
  phi_indirect <- simStateSpace:::as.data.frame.simstatespace(
    object$output$indirect
  )
  time <- phi[, "time"]
  phi_vec <- phi[, varnames]
  dim(phi_vec) <- NULL
  phi_direct_vec <- phi_direct[, varnames]
  dim(phi_direct_vec) <- NULL
  phi_indirect_vec <- phi_indirect[, varnames]
  dim(phi_indirect_vec) <- NULL
  phi <- phi[, varnames]
  phi_direct <- phi_direct[, varnames]
  phi_indirect <- phi_indirect[, varnames]
  col_direct <- "#2c7bb6"
  col_indirect <- "#d7191c"
  col_total <- "#5e3c99"
  for (i in seq_len(p)) {
    graphics::plot.default(
      x = 0,
      y = 0,
      xlim = range(time),
      ylim = range(c(phi_vec, phi_direct_vec, phi_indirect_vec)),
      type = "n",
      xlab = "Time",
      ylab = ylab[i],
      main = ""
    )
    graphics::abline(
      h = 0
    )
    if (total) {
      graphics::lines(
        x = time,
        y = phi[, i],
        type = "l",
        col = col_total,
        lty = 3,
        lwd = 2
      )
    }
    graphics::lines(
      x = time,
      y = phi_direct[, i],
      type = "l",
      col = col_direct,
      lty = 2,
      lwd = 2
    )
    graphics::lines(
      x = time,
      y = phi_indirect[, i],
      type = "l",
      col = col_indirect,
      lty = 1,
      lwd = 2
    )
    if (total) {
      graphics::legend(
        x = legend_pos,
        legend = c("Indirect", "Direct", "Total"),
        lty = 1:3,
        col = c(col_indirect, col_direct, col_total),
        cex = 0.8,
        lwd = 2
      )
    } else {
      graphics::legend(
        x = legend_pos,
        legend = c("Indirect", "Direct"),
        lty = 1:2,
        col = c(col_indirect, col_direct),
        cex = 0.8,
        lwd = 2
      )
    }
  }
  invisible(NULL)
}

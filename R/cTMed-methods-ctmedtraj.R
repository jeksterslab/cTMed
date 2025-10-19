#' Print Method for Object of Class `ctmedtraj`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedtraj`.
#' @param ... further arguments.
#' @param digits Integer indicating the number of decimal places to display.
#'
#' @return Prints a data frame of simulated data.
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
#' print(traj)
#'
#' @keywords methods
#' @export
print.ctmedtraj <- function(x,
                            digits = 4,
                            ...) {
  print.summary.ctmedtraj(
    summary.ctmedtraj(
      object = x,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `ctmedtraj`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `ctmedtraj`.
#' @param ... further arguments.
#' @param digits Integer indicating the number of decimal places to display.
#'
#' @return Returns a data frame of simulated data.
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
#' summary(traj)
#'
#' @keywords methods
#' @export
summary.ctmedtraj <- function(object,
                              digits = 4,
                              ...) {
  x <- object$output
  total <- simStateSpace:::as.data.frame.simstatespace(
    x$total
  )
  total$effect <- "total"
  direct <- simStateSpace:::as.data.frame.simstatespace(
    x$direct
  )
  direct$effect <- "direct"
  indirect <- simStateSpace:::as.data.frame.simstatespace(
    x$indirect
  )
  indirect$effect <- "indirect"
  out <- rbind(
    total,
    direct,
    indirect
  )
  out <- out[, -c(1)]
  print_summary <- out
  num_cols <- sapply(
    X = print_summary,
    FUN = is.numeric
  )
  print_summary[num_cols] <- lapply(
    X = print_summary[num_cols],
    FUN = round,
    digits = digits
  )
  attr(
    x = out,
    which = "fit"
  ) <- object
  attr(
    x = out,
    which = "print_summary"
  ) <- print_summary
  attr(
    x = out,
    which = "digits"
  ) <- digits
  class(out) <- "summary.ctmedtraj"
  out
}

#' @noRd
#' @keywords internal
#' @exportS3Method print summary.ctmedtraj
print.summary.ctmedtraj <- function(x,
                                    ...) {
  print_summary <- attr(
    x = x,
    which = "print_summary"
  )
  object <- attr(
    x = x,
    which = "fit"
  )
  cat("Call:\n")
  base::print(object$call)
  print(print_summary)
  invisible(x)
}

#' Plot Method for an Object of Class `ctmedtraj`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmedtraj`.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
#' @param total Logical.
#'   If `total = TRUE`, include the total effect trajectory.
#'   If `total = FALSE`, exclude the total effect trajectory.
#' @param ... Additional arguments.
#'
#' @return Displays trajectory plots of the effects.
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
#' @keywords methods
#' @export
plot.ctmedtraj <- function(x,
                           legend_pos = "topright",
                           total = TRUE,
                           ...) {
  .PlotTrajectory(
    object = x,
    legend_pos = legend_pos,
    total = total
  )
}

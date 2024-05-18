#' Print Method for Object of Class `ctmedtraj`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedtraj`.
#' @param ... further arguments.
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
#'   mu = c(3, 3, -3),
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
                            ...) {
  x <- x$output
  total <- simStateSpace:::as.data.frame.simstatespace(
    x$total
  )
  total$total <- TRUE
  direct <- simStateSpace:::as.data.frame.simstatespace(
    x$direct
  )
  direct$total <- FALSE
  out <- rbind(
    total,
    direct
  )
  return(
    out[, -c(2)]
  )
}

#' Summary Method for an Object of Class `ctmedtraj`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `ctmedtraj`.
#' @param ... further arguments.
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
#'   mu = c(3, 3, -3),
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
                              ...) {
  x <- object$output
  total <- simStateSpace:::as.data.frame.simstatespace(
    x$total
  )
  total$total <- TRUE
  direct <- simStateSpace:::as.data.frame.simstatespace(
    x$direct
  )
  direct$total <- FALSE
  out <- rbind(
    total,
    direct
  )
  return(
    out[, -c(1)]
  )
}

#' Plot Method for an Object of Class `ctmedtraj`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmedtraj`.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
#' @param ... Additional arguments.
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
#'   mu = c(3, 3, -3),
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
                           ...) {
  return(
    .PlotTrajectory(
      object = x,
      legend_pos = legend_pos
    )
  )
}
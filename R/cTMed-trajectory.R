#' Simulate Trajectories of Variables
#'
#' This function simulates trajectories of variables
#' without measurement error or process noise.
#' `Total` corresponds to the total effect
#' and `Direct` correponds to the portion of the total effect
#' where the indirect effect is removed.
#'
#' @inheritParams Indirect
#' @param mu0 Numeric vector.
#'   Initial values of the variables.
#' @param time Positive integer.
#'   Number of time points.
#'
#' @return Returns an object
#'   of class `ctmedtraj` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("Trajectory").}
#'     \item{output}{A data frame of simulated data.}
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
#'
#' traj <- Trajectory(
#'   mu = c(3, 3, -3),
#'   time = 150,
#'   phi = phi,
#'   med = "m"
#' )
#' plot(traj)
#'
#' # Methods -------------------------------------------------------------------
#' # Med has a number of methods including
#' # print, summary, and plot
#'
#' traj <- Trajectory(
#'   mu = c(3, 3, -3),
#'   time = 25,
#'   phi = phi,
#'   med = "m"
#' )
#' print(traj)
#' summary(traj)
#' plot(traj)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed effects path
#' @export
Trajectory <- function(mu0,
                       time,
                       phi,
                       med) {
  idx <- rownames(phi)
  p <- dim(phi)[1]
  stopifnot(
    idx == colnames(phi),
    length(mu0) == p
  )
  for (i in seq_len(length(med))) {
    stopifnot(
      med[i] %in% idx
    )
  }
  args <- list(
    mu0 = mu0,
    time = time,
    phi = phi,
    med = med
  )

  # zero on paths from and to med
  # except autoeffects
  phi_direct <- phi
  phi_direct[, med] <- 0
  phi_direct[med, ] <- 0
  diag(phi_direct) <- diag(phi)
  # generate data
  iden_mat <- diag(p)
  null_vec <- rep(x = 0, times = p)
  null_mat <- matrix(
    data = 0,
    nrow = p,
    ncol = p
  )
  total <- simStateSpace::SimSSMOUFixed(
    n = 1,
    time = time,
    delta_t = 0.10,
    mu0 = mu0,
    sigma0_l = null_mat,
    mu = null_vec,
    phi = phi,
    sigma_l = null_mat,
    nu = null_vec,
    lambda = iden_mat,
    theta_l = null_mat
  )
  direct <- simStateSpace::SimSSMOUFixed(
    n = 1,
    time = time,
    delta_t = 0.10,
    mu0 = mu0,
    sigma0_l = null_mat,
    mu = null_vec,
    phi = phi_direct,
    sigma_l = null_mat,
    nu = null_vec,
    lambda = iden_mat,
    theta_l = null_mat
  )
  output <- list(
    total = total,
    direct = direct
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "Trajectory",
    output = output
  )
  class(out) <- c(
    "ctmedtraj",
    class(out)
  )
  return(out)
}

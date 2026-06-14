#' Standardized Total Effect Centrality
#'
#' @details The standardized total effect centrality of a variable
#' is the sum of the standardized total effects of a variable
#' on all other variables at a particular time interval.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams TotalCentral
#' @inheritParams TotalStd
#'
#' @return Returns an object
#'   of class `ctmedmed` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("TotalCentralStd").}
#'     \item{output}{A matrix of standardized total effect centrality.}
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
#'     0.24455556, 0.02201587, -0.05004762,
#'     0.02201587, 0.07067800, 0.01539456,
#'     -0.05004762, 0.01539456, 0.07553061
#'   ),
#'   nrow = 3
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' TotalCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' total_central_std <- TotalCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:30
#' )
#' plot(total_central_std)
#'
#' # Methods -------------------------------------------------------------------
#' # TotalCentralStd has a number of methods including
#' # print, summary, and plot
#' total_central_std <- TotalCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:5
#' )
#' print(total_central_std)
#' summary(total_central_std)
#' plot(total_central_std)
#'
#' @family Continuous-Time Mediation Functions
#' @keywords cTMed network effects
#' @export
TotalCentralStd <- function(phi,
                            sigma,
                            delta_t,
                            tol = 0.001) {
  delta_t <- ifelse(
    test = delta_t < tol,
    yes = tol, # .Machine$double.xmin
    no = delta_t
  )
  args <- list(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t,
    network = TRUE,
    type = "total",
    standardized = TRUE
  )
  if (length(delta_t) > 1) {
    output <- .TotalCentralStds(
      phi = phi,
      sigma = sigma,
      delta_t = delta_t
    )
  } else {
    output <- matrix(
      data = c(
        .TotalCentralStd(
          phi = phi,
          sigma = sigma,
          delta_t = delta_t
        )
      ),
      nrow = 1
    )
  }
  output <- cbind(output, delta_t)
  colnames(output) <- c(colnames(phi), "interval")
  out <- list(
    call = match.call(),
    args = args,
    fun = "TotalCentralStd",
    output = output
  )
  class(out) <- c("ctmedmed", class(out))
  out
}

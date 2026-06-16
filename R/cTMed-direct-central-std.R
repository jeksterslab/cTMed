#' Standardized Direct Effect Centrality
#'
#' @details Standardized direct effect centrality
#' is the sum of all possible standardized direct effects
#' between different pairs of variables
#' in which a specific variable serves as the only mediator.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams DirectCentral
#' @inheritParams DirectStd
#'
#' @return Returns an object
#'   of class `ctmedmed` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("DirectCentralStd").}
#'     \item{output}{A matrix of standardized direct effect centrality.}
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
#' DirectCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' direct_central_std <- DirectCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:30
#' )
#' plot(direct_central_std)
#'
#' # Methods -------------------------------------------------------------------
#' # DirectCentralStd has a number of methods including
#' # print, summary, and plot
#' direct_central_std <- DirectCentralStd(
#'   phi = phi,
#'   sigma = sigma,
#'   delta_t = 1:5
#' )
#' print(direct_central_std)
#' summary(direct_central_std)
#' plot(direct_central_std)
#'
#' @family Continuous-Time Mediation Functions
#' @keywords cTMed network effects
#' @export
DirectCentralStd <- function(phi,
                             sigma,
                             delta_t,
                             tol = 0.001) {
  delta_t <- sort(
    unique(
      pmax(
        delta_t,
        tol
      )
    )
  )
  args <- list(
    phi = phi,
    sigma = sigma,
    delta_t = delta_t,
    network = TRUE,
    type = "direct",
    standardized = TRUE
  )
  if (length(delta_t) > 1) {
    output <- .DirectCentralStds(
      phi = phi,
      sigma = sigma,
      delta_t = delta_t
    )
  } else {
    output <- matrix(
      data = c(
        .DirectCentralStd(
          phi = phi,
          sigma = sigma,
          delta_t = delta_t
        )
      ),
      nrow = 1
    )
  }
  output <- cbind(
    output,
    delta_t
  )
  colnames(output) <- c(
    colnames(phi),
    "interval"
  )
  out <- list(
    call = match.call(),
    args = args,
    fun = "DirectCentralStd",
    output = output
  )
  class(out) <- c(
    "ctmedmed",
    class(out)
  )
  out
}

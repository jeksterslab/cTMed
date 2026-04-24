#' Direct Effect Centrality
#'
#' @details Direct effect centrality
#' is the sum of all possible direct effects
#' between different pairs of variables
#' in which a specific variable serves as the only mediator.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams Med
#' @inherit Direct references
#'
#' @return Returns an object
#'   of class `ctmedmed` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("DirectCentral").}
#'     \item{output}{A matrix of direct effect centrality.}
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
#' # Specific time interval ----------------------------------------------------
#' DirectCentral(
#'   phi = phi,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' direct_central <- DirectCentral(
#'   phi = phi,
#'   delta_t = 1:30
#' )
#' plot(direct_central)
#'
#' # Methods -------------------------------------------------------------------
#' # DirectCentral has a number of methods including
#' # print, summary, and plot
#' direct_central <- DirectCentral(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' print(direct_central)
#' summary(direct_central)
#' plot(direct_central)
#'
#' @family Continuous-Time Mediation Functions
#' @keywords cTMed network effects
#' @export
DirectCentral <- function(phi,
                          delta_t,
                          tol = 0.01) {
  delta_t <- ifelse(
    test = delta_t < tol,
    yes = tol, # .Machine$double.xmin
    no = delta_t
  )
  args <- list(
    phi = phi,
    delta_t = delta_t,
    network = TRUE,
    centrality = "direct"
  )
  if (length(delta_t) > 1) {
    output <- .DirectCentrals(
      phi = phi,
      delta_t = delta_t
    )
  } else {
    output <- matrix(
      data = c(
        .DirectCentral(
          phi = phi,
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
    fun = "DirectCentral",
    output = output
  )
  class(out) <- c(
    "ctmedmed",
    class(out)
  )
  out
}

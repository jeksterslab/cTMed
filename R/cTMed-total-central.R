#' Total Effect Centrality
#'
#' @details The total effect centrality of a variable
#' is the sum of the total effects of a variable on all other variables
#' at a particular time interval.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams Med
#' @inherit Indirect references
#'
#' @return Returns an object
#'   of class `ctmedmed` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("TotalCentral").}
#'     \item{output}{A matrix of total effect centrality.}
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
#' TotalCentral(
#'   phi = phi,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' total_central <- TotalCentral(
#'   phi = phi,
#'   delta_t = 1:30
#' )
#' plot(total_central)
#'
#' # Methods -------------------------------------------------------------------
#' # TotalCentral has a number of methods including
#' # print, summary, and plot
#' total_central <- TotalCentral(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' print(total_central)
#' summary(total_central)
#' plot(total_central)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed network effects
#' @export
TotalCentral <- function(phi,
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
    total = TRUE
  )
  if (length(delta_t) > 1) {
    output <- .TotalCentrals(
      phi = phi,
      delta_t = delta_t
    )
  } else {
    output <- matrix(
      data = c(
        .TotalCentral(
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
    fun = "TotalCentral",
    output = output
  )
  class(out) <- c(
    "ctmedmed",
    class(out)
  )
  out
}

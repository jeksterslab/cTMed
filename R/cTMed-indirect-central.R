#' Indirect Effect Centrality
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams Med
#'
#' @return Returns an object
#'   of class `ctmedmed` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("IndirectCentral").}
#'     \item{output}{A matrix of indirect effect centrality.}
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
#' IndirectCentral(
#'   phi = phi,
#'   delta_t = 1
#' )
#'
#' # Range of time intervals ---------------------------------------------------
#' indirect_central <- IndirectCentral(
#'   phi = phi,
#'   delta_t = 1:30
#' )
#' plot(indirect_central)
#'
#' # Methods -------------------------------------------------------------------
#' # IndirectCentral has a number of methods including
#' # print, summary, and plot
#' indirect_central <- IndirectCentral(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' print(indirect_central)
#' summary(indirect_central)
#' plot(indirect_central)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed network
#' @export
IndirectCentral <- function(phi,
                            delta_t) {
  delta_t <- ifelse(
    test = delta_t <= 0,
    yes = .Machine$double.xmin,
    no = delta_t
  )
  args <- list(
    phi = phi,
    delta_t = delta_t,
    network = TRUE,
    total = FALSE
  )
  if (length(delta_t) > 1) {
    output <- .IndirectCentrals(
      phi = phi,
      delta_t = delta_t
    )
  } else {
    output <- matrix(
      data = c(
        .IndirectCentral(
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
    fun = "IndirectCentral",
    output = output
  )
  class(out) <- c(
    "ctmedmed",
    class(out)
  )
  return(out)
}

#' Summary Method for Object of Class `ctmedposteriorphi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `ctmedposteriorphi`.
#' @param ... further arguments.
#' @return Returns a list of the posterior means
#'   (in matrix form) and covariance matrix.
#'
#' @examples
#' set.seed(42)
#' phi <- matrix(
#'   data = c(
#'     -0.357, 0.771, -0.450,
#'     0.0, -0.511, 0.729,
#'     0, 0, -0.693
#'   ),
#'   nrow = 3
#' )
#' colnames(phi) <- rownames(phi) <- c("x", "m", "y")
#' mc <- MCPhi(
#'   phi = phi,
#'   vcov_phi_vec = 0.1 * diag(9),
#'   R = 100L # use a large value for R in actual research
#' )$output
#' summary(mc)
#'
#' @keywords methods
#' @export
summary.ctmedposteriorphi <- function(object,
                                      ...) {
  varnames <- colnames(object[[1]])
  p <- dim(object[[1]])[1]
  x <- do.call(
    what = "rbind",
    args = lapply(
      X = object,
      FUN = function(x) {
        dim(x) <- NULL
        return(x)
      }
    )
  )
  phi_vec <- colMeans(x)
  phi <- matrix(
    data = phi_vec,
    nrow = p
  )
  colnames(phi) <- rownames(phi) <- varnames
  vcov_phi_vec <- stats::var(x)
  return(
    list(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec
    )
  )
}

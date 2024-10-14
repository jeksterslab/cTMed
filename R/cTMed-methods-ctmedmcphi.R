#' Print Method for Object of Class `ctmedmcphi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedmcphi`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#'
#' @return Prints a list of drift matrices.
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
#' )
#' print(mc)
#'
#' @keywords methods
#' @export
print.ctmedmcphi <- function(x,
                             digits = 4,
                             ...) {
  base::print(
    lapply(
      X = x$output,
      FUN = round,
      digits = digits
    )
  )
}

#' Summary Method for Object of Class `ctmedposteriorphi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `ctmedposteriorphi`.
#' @param ... further arguments.
#' @return Returns a list of the posterior means
#'   (in matrix form) and covariance matrix.
#'
#' @keywords methods
#' @export
summary.ctmedposteriorphi <- function(object, # nocov start
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
} # nocov end

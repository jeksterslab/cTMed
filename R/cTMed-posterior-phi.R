#' Extract the Posterior Samples of the Drift Matrix
#'
#' The function extracts the posterior samples
#' of the drift matrix from a fitted model
#' from the [ctsem::ctStanFit()] function.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `ctStanFit`.
#'   Output of the [ctsem::ctStanFit()] function.
#'
#' @return Returns an object
#'   of class `ctmedposteriorphi` which is a list drift matrices
#'   sampled from the posterior distribution.
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed posterior
#' @export
PosteriorPhi <- function(object) { # nocov start
  stopifnot(
    inherits(
      x = object,
      what = "ctStanFit"
    )
  )
  varnames <- object$ctstanmodelbase$latentNames
  posterior <- ctsem::ctExtract(object)$pop_DRIFT
  out <- lapply(
    X = seq_len(dim(posterior)[1]),
    FUN = function(i) {
      phi <- posterior[i, , ]
      colnames(phi) <- rownames(phi) <- varnames
      return(phi)
    }
  )
  class(out) <- c(
    "ctmedposteriorphi",
    class(out)
  )
  return(out)
} # nocov end

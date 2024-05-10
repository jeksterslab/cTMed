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
#' @family Continuous Time Mediation Functions
#' @keywords cTMed uncertainty
#' @export
PosteriorPhi <- function(object) {
  stopifnot(
    inherits(
      object,
      "ctStanFit"
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
}

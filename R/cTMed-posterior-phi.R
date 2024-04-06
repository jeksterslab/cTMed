#' Extract the Posterior Samples of the Drift Matrix
#'
#' The function extracts the posterior samples
#' of the drift matrix from a fitted model
#' using the [ctsem::ctStanFit()] function.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctStanFit`.
#'   Output of the [ctsem::ctStanFit()] function.
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed uncertainty
#' @export
PosteriorPhi <- function(x) {
  stopifnot(
    inherits(
      object,
      "ctStanFit"
    )
  )
  varnames <- x$ctstanmodelbase$latentNames
  posterior <- ctsem::ctExtract(x)$pop_DRIFT
  return(
    lapply(
      X = seq_len(dim(posterior)[1]),
      FUN = function(i) {
        phi <- posterior[i, , ]
        colnames(phi) <- rownames(phi) <- varnames
        return(phi)
      }
    )
  )
}

#' Effect Sizes for the Direct and Indirect Effects of X on Y
#' Through M
#' Over a Specific Time Interval
#' or a Range of Time Intervals
#'
#' This function computes effect sizes
#' for the direct and indirect effects of X on Y
#' through M
#' over a specific time interval
#' or a range of time intervals
#' with corresponding confidence intervals.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [DeltaMed()] or [MCMed()] functions.
#' @param what Character string.
#'   Type of effect size measure.
#'   If `what = "ratio"`, the following effect sizes are calculated
#'   \eqn{\mathrm{Direct} / \mathrm{Total}} and
#'   \eqn{\mathrm{Indirect} / \mathrm{Total}}.
#'   If `what = "proportion"`, the following effect sizes are calculated
#'   \eqn{\mathrm{Direct} / (\mathrm{Direct} + \mathrm{Total})} and
#'   \eqn{\mathrm{Indirect} / (\mathrm{Indirect} + \mathrm{Total})}.
#' @inheritParams DeltaMed
#'
#' @return Returns an object
#'   of class `ctesdelta` or `ctesmc` depending on `object`.
#'   `ctesdelta` is a lists with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("DeltaRatio" or "DeltaProp").}
#'     \item{output}{A list with length of `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{delta_t}{Time interval.}
#'     \item{jacobian}{Jacobian matrix.}
#'     \item{est}{Estimated effect sizes.}
#'     \item{vcov}{Sampling variance-covariance matrix of the
#'     effect sizes.}
#'   }
#'   `ctesmc` is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{fun}{Function used ("MCRatio" or "MCProp").}
#'     \item{output}{A list with length of `length(delta_t)`.}
#'   }
#'   Each element in the `output` list has the following elements:
#'   \describe{
#'     \item{est}{A vector of effect sizes.}
#'     \item{thetahatstar}{A matrix of Monte Carlo
#'     effect sizes.}
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
#' vcov_phi_vec <- matrix(
#'   data = c(
#'     0.002704274, -0.001475275, 0.000949122,
#'     -0.001619422, 0.000885122, -0.000569404,
#'     0.00085493, -0.000465824, 0.000297815,
#'     -0.001475275, 0.004428442, -0.002642303,
#'     0.000980573, -0.00271817, 0.001618805,
#'     -0.000586921, 0.001478421, -0.000871547,
#'     0.000949122, -0.002642303, 0.006402668,
#'     -0.000697798, 0.001813471, -0.004043138,
#'     0.000463086, -0.001120949, 0.002271711,
#'     -0.001619422, 0.000980573, -0.000697798,
#'     0.002079286, -0.001152501, 0.000753,
#'     -0.001528701, 0.000820587, -0.000517524,
#'     0.000885122, -0.00271817, 0.001813471,
#'     -0.001152501, 0.00342605, -0.002075005,
#'     0.000899165, -0.002532849, 0.001475579,
#'     -0.000569404, 0.001618805, -0.004043138,
#'     0.000753, -0.002075005, 0.004984032,
#'     -0.000622255, 0.001634917, -0.003705661,
#'     0.00085493, -0.000586921, 0.000463086,
#'     -0.001528701, 0.000899165, -0.000622255,
#'     0.002060076, -0.001096684, 0.000686386,
#'     -0.000465824, 0.001478421, -0.001120949,
#'     0.000820587, -0.002532849, 0.001634917,
#'     -0.001096684, 0.003328692, -0.001926088,
#'     0.000297815, -0.000871547, 0.002271711,
#'     -0.000517524, 0.001475579, -0.003705661,
#'     0.000686386, -0.001926088, 0.004726235
#'   ),
#'   nrow = 9
#' )
#'
#' # Delta Method --------------------------------------------------------------
#' # Specific time interval ----------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' EffectSizeMed(object = delta, what = "ratio")
#' EffectSizeMed(object = delta, what = "proportion")
#'
#' # Range of time intervals ---------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' es <- EffectSizeMed(object = delta, what = "ratio")
#' plot(es)
#' es <- EffectSizeMed(object = delta, what = "proportion")
#' plot(es)
#'
#' # Methods -------------------------------------------------------------------
#' # EffectSizeMed has a number of methods including
#' # print, summary, confint, and plot
#' print(es)
#' summary(es)
#' confint(es, level = 0.95)
#' plot(es)
#'
#' # MC Method -----------------------------------------------------------------
#' # Specific time interval ----------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 100L # use a large value for R in actual research
#' )
#' EffectSizeMed(object = mc, what = "ratio")
#' EffectSizeMed(object = mc, what = "proportion")
#'
#' # Range of time intervals ---------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 100L # use a large value for R in actual research
#' )
#' es <- EffectSizeMed(object = mc, what = "ratio")
#' plot(es)
#' es <- EffectSizeMed(object = mc, what = "proportion")
#' plot(es)
#'
#' # Methods -------------------------------------------------------------------
#' # EffectSizeMed has a number of methods including
#' # print, summary, confint, and plot
#' print(es)
#' summary(es)
#' confint(es, level = 0.95)
#' plot(es)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed uncertainty path
#' @export
EffectSizeMed <- function(object,
                          what = "ratio",
                          ncores = NULL) {
  if (object$fun == "DeltaMed") {
    if (what == "proportion") {
      return(
        DeltaProp(
          phi = object$args$phi,
          vcov_phi_vec = object$args$vcov_phi_vec,
          delta_t = object$args$delta_t,
          from = object$args$from,
          to = object$args$to,
          med = object$args$med,
          ncores = ncores
        )
      )
    }
    if (what == "ratio") {
      return(
        DeltaRatio(
          phi = object$args$phi,
          vcov_phi_vec = object$args$vcov_phi_vec,
          delta_t = object$args$delta_t,
          from = object$args$from,
          to = object$args$to,
          med = object$args$med,
          ncores = ncores
        )
      )
    }
  }
  if (object$fun == "MCMed") {
    if (what == "proportion") {
      return(
        MCProp(
          object = object,
          ncores = ncores
        )
      )
    }
    if (what == "ratio") {
      return(
        MCRatio(
          object = object,
          ncores = ncores
        )
      )
    }
  }
}

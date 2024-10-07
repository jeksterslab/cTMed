#' Print Method for Object of Class `ctmedeffect`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedeffect`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns the effects.
#' @return Displays plots of the point estimates and confidence intervals.
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
#' delta_t <- 1
#'
#' # Time Interval of One -----------------------------------------------------
#'
#' ## Total Effect ------------------------------------------------------------
#' total_dt <- Total(
#'   phi = phi,
#'   delta_t = delta_t
#' )
#' print(total_dt)
#'
#' ## Direct Effect -----------------------------------------------------------
#' direct_dt <- Direct(
#'   phi = phi,
#'   delta_t = delta_t,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(direct_dt)
#'
#' ## Indirect Effect ---------------------------------------------------------
#' indirect_dt <- Indirect(
#'   phi = phi,
#'   delta_t = delta_t,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(indirect_dt)
#'
#' @keywords methods
#' @export
print.ctmedeffect <- function(x,
                              digits = 4,
                              ...) {
  base::print(
    round(
      x$output,
      digits = digits
    )
  )
}

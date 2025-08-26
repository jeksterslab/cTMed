#' Print Method for Object of Class `ctmeddelta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmeddelta`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#'
#' @return Prints a list of matrices of
#'   time intervals,
#'   estimates,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
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
#'     0.00843, 0.00040, -0.00151,
#'     -0.00600, -0.00033, 0.00110,
#'     0.00324, 0.00020, -0.00061,
#'     0.00040, 0.00374, 0.00016,
#'     -0.00022, -0.00273, -0.00016,
#'     0.00009, 0.00150, 0.00012,
#'     -0.00151, 0.00016, 0.00389,
#'     0.00103, -0.00007, -0.00283,
#'     -0.00050, 0.00000, 0.00156,
#'     -0.00600, -0.00022, 0.00103,
#'     0.00644, 0.00031, -0.00119,
#'     -0.00374, -0.00021, 0.00070,
#'     -0.00033, -0.00273, -0.00007,
#'     0.00031, 0.00287, 0.00013,
#'     -0.00014, -0.00170, -0.00012,
#'     0.00110, -0.00016, -0.00283,
#'     -0.00119, 0.00013, 0.00297,
#'     0.00063, -0.00004, -0.00177,
#'     0.00324, 0.00009, -0.00050,
#'     -0.00374, -0.00014, 0.00063,
#'     0.00495, 0.00024, -0.00093,
#'     0.00020, 0.00150, 0.00000,
#'     -0.00021, -0.00170, -0.00004,
#'     0.00024, 0.00214, 0.00012,
#'     -0.00061, 0.00012, 0.00156,
#'     0.00070, -0.00012, -0.00177,
#'     -0.00093, 0.00012, 0.00223
#'   ),
#'   nrow = 9
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(delta)
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
#' print(delta)
#'
#' @keywords methods
#' @export
print.ctmeddelta <- function(x,
                             alpha = 0.05,
                             digits = 4,
                             ...) {
  if (x$args$network) {
    if (x$args$total) {
      cat(
        paste0(
          "\nTotal Effect Centrality\n\n"
        )
      )
    } else {
      cat(
        paste0(
          "\nIndirect Effect Centrality\n\n"
        )
      )
    }
  } else {
    if (x$fun == "DeltaMed" || x$fun == "DeltaMedStd") {
      cat(
        paste0(
          "\nTotal, Direct, and Indirect Effects\n\n"
        )
      )
    } else {
      cat(
        paste0(
          "\nElements of the matrix of lagged coefficients\n\n"
        )
      )
    }
  }
  base::print(
    lapply(
      X = .DeltaCI(
        object = x,
        alpha = alpha
      ),
      FUN = round,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `ctmeddelta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a data frame of
#'   effects,
#'   time intervals,
#'   estimates,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `ctmeddelta`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
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
#'     0.00843, 0.00040, -0.00151,
#'     -0.00600, -0.00033, 0.00110,
#'     0.00324, 0.00020, -0.00061,
#'     0.00040, 0.00374, 0.00016,
#'     -0.00022, -0.00273, -0.00016,
#'     0.00009, 0.00150, 0.00012,
#'     -0.00151, 0.00016, 0.00389,
#'     0.00103, -0.00007, -0.00283,
#'     -0.00050, 0.00000, 0.00156,
#'     -0.00600, -0.00022, 0.00103,
#'     0.00644, 0.00031, -0.00119,
#'     -0.00374, -0.00021, 0.00070,
#'     -0.00033, -0.00273, -0.00007,
#'     0.00031, 0.00287, 0.00013,
#'     -0.00014, -0.00170, -0.00012,
#'     0.00110, -0.00016, -0.00283,
#'     -0.00119, 0.00013, 0.00297,
#'     0.00063, -0.00004, -0.00177,
#'     0.00324, 0.00009, -0.00050,
#'     -0.00374, -0.00014, 0.00063,
#'     0.00495, 0.00024, -0.00093,
#'     0.00020, 0.00150, 0.00000,
#'     -0.00021, -0.00170, -0.00004,
#'     0.00024, 0.00214, 0.00012,
#'     -0.00061, 0.00012, 0.00156,
#'     0.00070, -0.00012, -0.00177,
#'     -0.00093, 0.00012, 0.00223
#'   ),
#'   nrow = 9
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' summary(delta)
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
#' summary(delta)
#'
#' @keywords methods
#' @export
summary.ctmeddelta <- function(object,
                               alpha = 0.05,
                               ...) {
  if (object$args$network) {
    if (object$args$total) {
      if (interactive()) {
        # nocov start
        cat(
          paste0(
            "\nTotal Effect Centrality\n\n"
          )
        )
        # nocov end
      }
    } else {
      if (interactive()) {
        # nocov start
        cat(
          paste0(
            "\nIndirect Effect Centrality\n\n"
          )
        )
        # nocov end
      }
    }
  } else {
    if (interactive()) {
      # nocov start
      if (object$fun == "DeltaMed" || object$fun == "DeltaMedStd") {
        cat(
          paste0(
            "\nTotal, Direct, and Indirect Effects\n\n"
          )
        )
      } else {
        cat(
          paste0(
            "\nElements of the matrix of lagged coefficients\n\n"
          )
        )
      }
      # nocov end
    }
  }
  ci <- .DeltaCI(
    object = object,
    alpha = alpha
  )
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  effect <- rownames(ci)
  ci <- as.data.frame(
    ci
  )
  if (object$args$network) {
    ci$variable <- effect
  } else {
    ci$effect <- effect
  }
  rownames(ci) <- NULL
  varnames <- colnames(ci)
  p <- dim(ci)[2]
  varnames <- varnames[c(p, 1:(p - 1))]
  ci[, varnames]
}

#' Delta Method Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `ctmeddelta`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @return Returns a data frame of confidence intervals.
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
#'     0.00843, 0.00040, -0.00151,
#'     -0.00600, -0.00033, 0.00110,
#'     0.00324, 0.00020, -0.00061,
#'     0.00040, 0.00374, 0.00016,
#'     -0.00022, -0.00273, -0.00016,
#'     0.00009, 0.00150, 0.00012,
#'     -0.00151, 0.00016, 0.00389,
#'     0.00103, -0.00007, -0.00283,
#'     -0.00050, 0.00000, 0.00156,
#'     -0.00600, -0.00022, 0.00103,
#'     0.00644, 0.00031, -0.00119,
#'     -0.00374, -0.00021, 0.00070,
#'     -0.00033, -0.00273, -0.00007,
#'     0.00031, 0.00287, 0.00013,
#'     -0.00014, -0.00170, -0.00012,
#'     0.00110, -0.00016, -0.00283,
#'     -0.00119, 0.00013, 0.00297,
#'     0.00063, -0.00004, -0.00177,
#'     0.00324, 0.00009, -0.00050,
#'     -0.00374, -0.00014, 0.00063,
#'     0.00495, 0.00024, -0.00093,
#'     0.00020, 0.00150, 0.00000,
#'     -0.00021, -0.00170, -0.00004,
#'     0.00024, 0.00214, 0.00012,
#'     -0.00061, 0.00012, 0.00156,
#'     0.00070, -0.00012, -0.00177,
#'     -0.00093, 0.00012, 0.00223
#'   ),
#'   nrow = 9
#' )
#'
#' # Specific time interval ----------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' confint(delta, level = 0.95)
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
#' confint(delta, level = 0.95)
#'
#' @keywords methods
#' @export
confint.ctmeddelta <- function(object,
                               parm = NULL,
                               level = 0.95,
                               ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$output[[1]]$est)
    )
  }
  ci <- .DeltaCI(
    object = object,
    alpha = 1 - level[1]
  )
  ci <- lapply(
    X = ci,
    FUN = function(i) {
      out <- i[parm, c(1, 6, 7), drop = FALSE] # always z
      varnames <- colnames(out)
      varnames <- gsub(
        pattern = "%",
        replacement = " %",
        x = varnames
      )
      colnames(out) <- varnames
      out
    }
  )
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  effect <- rownames(ci)
  ci <- as.data.frame(
    ci
  )
  if (object$args$network) {
    ci$variable <- effect
  } else {
    ci$effect <- effect
  }
  rownames(ci) <- NULL
  varnames <- colnames(ci)
  p <- dim(ci)[2]
  varnames <- varnames[c(p, 1:(p - 1))]
  ci[, varnames]
}

#' Plot Method for an Object of Class `ctmeddelta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmeddelta`.
#' @param alpha Numeric.
#'   Significance level
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param ... Additional arguments.
#'
#' @return Displays plots of point estimates and confidence intervals.
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
#'     0.00843, 0.00040, -0.00151,
#'     -0.00600, -0.00033, 0.00110,
#'     0.00324, 0.00020, -0.00061,
#'     0.00040, 0.00374, 0.00016,
#'     -0.00022, -0.00273, -0.00016,
#'     0.00009, 0.00150, 0.00012,
#'     -0.00151, 0.00016, 0.00389,
#'     0.00103, -0.00007, -0.00283,
#'     -0.00050, 0.00000, 0.00156,
#'     -0.00600, -0.00022, 0.00103,
#'     0.00644, 0.00031, -0.00119,
#'     -0.00374, -0.00021, 0.00070,
#'     -0.00033, -0.00273, -0.00007,
#'     0.00031, 0.00287, 0.00013,
#'     -0.00014, -0.00170, -0.00012,
#'     0.00110, -0.00016, -0.00283,
#'     -0.00119, 0.00013, 0.00297,
#'     0.00063, -0.00004, -0.00177,
#'     0.00324, 0.00009, -0.00050,
#'     -0.00374, -0.00014, 0.00063,
#'     0.00495, 0.00024, -0.00093,
#'     0.00020, 0.00150, 0.00000,
#'     -0.00021, -0.00170, -0.00004,
#'     0.00024, 0.00214, 0.00012,
#'     -0.00061, 0.00012, 0.00156,
#'     0.00070, -0.00012, -0.00177,
#'     -0.00093, 0.00012, 0.00223
#'   ),
#'   nrow = 9
#' )
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
#' plot(delta)
#'
#' @keywords methods
#' @export
plot.ctmeddelta <- function(x,
                            alpha = 0.05,
                            col = NULL,
                            ...) {
  if (x$args$network) {
    out <- .PlotCentralCI(
      object = x,
      alpha = alpha,
      col = col
    )
  } else {
    if (x$fun == "DeltaMed" || x$fun == "DeltaMedStd") {
      out <- .PlotMedCI(
        object = x,
        alpha = alpha,
        col = col
      )
    }
    if (x$fun == "DeltaBeta" || x$fun == "DeltaBetaStd") {
      out <- .PlotBetaCI(
        object = x,
        alpha = alpha,
        col = col
      )
    }
  }
  out
}

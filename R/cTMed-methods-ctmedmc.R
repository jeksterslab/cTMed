#' Print Method for Object of Class `ctmedmc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedmc`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#'
#' @return Prints a list of matrices of
#'   time intervals,
#'   estimates,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
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
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 100L # use a large value for R in actual research
#' )
#' print(mc)
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
#' print(mc)
#'
#' @keywords methods
#' @export
print.ctmedmc <- function(x,
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
    cat(
      paste0(
        "\nTotal, Direct, and Indirect Effects\n\n"
      )
    )
  }
  base::print(
    lapply(
      X = .MCCI(
        object = x,
        alpha = alpha
      ),
      FUN = round,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `ctmedmc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `ctmedmc`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @return Returns a data frame of
#'   effects,
#'   time intervals,
#'   estimates,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
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
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 100L # use a large value for R in actual research
#' )
#' summary(mc)
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
#' summary(mc)
#'
#' @keywords methods
#' @export
summary.ctmedmc <- function(object,
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
      cat(
        paste0(
          "\nTotal, Direct, and Indirect Effects\n\n"
        )
      )
      # nocov end
    }
  }
  ci <- .MCCI(
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

#' Monte Carlo Method Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `ctmedmc`.
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
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 100L # use a large value for R in actual research
#' )
#' confint(mc, level = 0.95)
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
#' confint(mc, level = 0.95)
#'
#' @keywords methods
#' @export
confint.ctmedmc <- function(object,
                            parm = NULL,
                            level = 0.95,
                            ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$output[[1]]$est[1:3])
    )
  }
  ci <- .MCCI(
    object = object,
    alpha = 1 - level[1]
  )
  ci <- lapply(
    X = ci,
    FUN = function(i) {
      out <- i[parm, c(1, 5, 6), drop = FALSE] # always z
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

#' Plot Method for an Object of Class `ctmedmc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmedmc`.
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
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 100L # use a large value for R in actual research
#' )
#' plot(mc)
#'
#' @keywords methods
#' @export
plot.ctmedmc <- function(x,
                         alpha = 0.05,
                         col = NULL,
                         ...) {
  if (x$args$network) {
    .PlotCentralCI(
      object = x,
      alpha = alpha,
      col = col
    )
  } else {
    if (x$fun %in% c("MCMed", "MCMedStd", "PosteriorMed", "BootMed")) {
      .PlotMedCI(
        object = x,
        alpha = alpha,
        col = col
      )
    }
    if (x$fun %in% c("MCBeta", "MCBetaStd", "PosteriorBeta")) {
      .PlotBetaCI(
        object = x,
        alpha = alpha,
        col = col
      )
    }
  }
}

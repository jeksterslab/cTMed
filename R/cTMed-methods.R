#' Print Method for Object of Class `ctmedeffect`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedeffect`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns the effects.
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
#' # Time-Interval of One -----------------------------------------------------
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

#' Print Method for Object of Class `ctmedmed`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedmed`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of effects.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Example 1 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
#' phi <- matrix(
#'   data = c(
#'     -0.357, 0.771, -0.450,
#'     0.0, -0.511, 0.729,
#'     0, 0, -0.693
#'   ),
#'   nrow = 3
#' )
#' colnames(phi) <- rownames(phi) <- c("x", "m", "y")
#'
#' # Specific time-interval ----------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(med)
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(med)
#'
#' # ---------------------------------------------------------------------------
#' # Example 2 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
#' phi <- matrix(
#'   data = c(
#'     -6, 5.5, 0, 0,
#'     1.25, -2.5, 5.9, -7.3,
#'     0, 0, -6, 2.5,
#'     5, 0, 0, -6
#'   ),
#'   nrow = 4
#' )
#' colnames(phi) <- rownames(phi) <- paste0("y", 1:4)
#'
#' # Specific time-interval ----------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "y2",
#'   to = "y4",
#'   med = c("y1", "y3")
#' )
#' print(med)
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:10,
#'   from = "y2",
#'   to = "y4",
#'   med = c("y1", "y3")
#' )
#' print(med)
#'
#' @keywords methods
#' @export
print.ctmedmed <- function(x,
                           digits = 4,
                           ...) {
  base::print(
    round(
      x$output[
        ,
        c(
          "interval",
          "total",
          "direct",
          "indirect"
        ),
        drop = FALSE
      ],
      digits = digits
    )
  )
}

#' Print Method for Object of Class `ctmedmcphi`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedmcphi`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns the structure of the output.
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
#' mc <- MCPhi(
#'   phi = phi,
#'   vcov_phi_vec = 0.1 * diag(9),
#'   R = 5
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

#' Print Method for Object of Class `ctmeddelta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmeddelta`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of
#'   time-interval,
#'   estimates,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Specific time-interval ----------------------------------------------------
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
#' # Range of time-intervals ---------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
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

#' Print Method for Object of Class `ctmedmc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedmc`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of estimates, standard errors,
#'   number of Monte Carlo replications, and confidence intervals.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Specific time-interval ----------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' print(mc)
#'
#' # Range of time-intervals ---------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' print(mc)
#'
#' @keywords methods
#' @export
print.ctmedmc <- function(x,
                          alpha = 0.05,
                          digits = 4,
                          ...) {
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

#' Summary Method for an Object of Class `ctmedmed`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param object an object of class `ctmedmed`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#' @return Returns a matrix of effects.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Example 1 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
#' phi <- matrix(
#'   data = c(
#'     -0.357, 0.771, -0.450,
#'     0.0, -0.511, 0.729,
#'     0, 0, -0.693
#'   ),
#'   nrow = 3
#' )
#' colnames(phi) <- rownames(phi) <- c("x", "m", "y")
#'
#' # Specific time-interval ----------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' summary(med)
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' summary(med)
#'
#' # ---------------------------------------------------------------------------
#' # Example 2 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
#' phi <- matrix(
#'   data = c(
#'     -6, 5.5, 0, 0,
#'     1.25, -2.5, 5.9, -7.3,
#'     0, 0, -6, 2.5,
#'     5, 0, 0, -6
#'   ),
#'   nrow = 4
#' )
#' colnames(phi) <- rownames(phi) <- paste0("y", 1:4)
#'
#' # Specific time-interval ----------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "y2",
#'   to = "y4",
#'   med = c("y1", "y3")
#' )
#' summary(med)
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:10,
#'   from = "y2",
#'   to = "y4",
#'   med = c("y1", "y3")
#' )
#' summary(med)
#'
#' @keywords methods
#' @export
summary.ctmedmed <- function(object,
                             digits = 4,
                             ...) {
  return(
    round(
      object$output[
        ,
        c(
          "interval",
          "total",
          "direct",
          "indirect"
        ),
        drop = FALSE
      ],
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `ctmeddelta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   effects,
#'   time-interval,
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
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Specific time-interval ----------------------------------------------------
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
#' # Range of time-intervals ---------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
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
  ci$effect <- effect
  rownames(ci) <- NULL
  varnames <- colnames(ci)
  p <- dim(ci)[2]
  varnames <- varnames[c(p, 1:(p - 1))]
  return(ci[, varnames])
}

#' Summary Method for an Object of Class `ctmedmc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   effects,
#'   time-interval,
#'   estimates,
#'   standard errors,
#'   test statistics,
#'   p-values,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `ctmedmc`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Specific time-interval ----------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' summary(mc)
#'
#' # Range of time-intervals ---------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' summary(mc)
#'
#' @keywords methods
#' @export
summary.ctmedmc <- function(object,
                            alpha = 0.05,
                            ...) {
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
  ci$effect <- effect
  rownames(ci) <- NULL
  varnames <- colnames(ci)
  p <- dim(ci)[2]
  varnames <- varnames[c(p, 1:(p - 1))]
  return(ci[, varnames])
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
#' @return Returns a matrix of confidence intervals.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Specific time-interval ----------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' confint(delta)
#'
#' # Range of time-intervals ---------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' confint(delta)
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
      return(
        out
      )
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
  ci$effect <- effect
  rownames(ci) <- NULL
  varnames <- colnames(ci)
  p <- dim(ci)[2]
  varnames <- varnames[c(p, 1:(p - 1))]
  return(ci[, varnames])
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
#' @return Returns a matrix of confidence intervals.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Specific time-interval ----------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' confint(mc)
#'
#' # Range of time-intervals ---------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' confint(mc)
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
      return(
        out
      )
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
  ci$effect <- effect
  rownames(ci) <- NULL
  varnames <- colnames(ci)
  p <- dim(ci)[2]
  varnames <- varnames[c(p, 1:(p - 1))]
  return(ci[, varnames])
}

#' Plot Method for an Object of Class `ctmedmed`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmedmed`.
#' @param ... Additional arguments.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Example 1 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
#' phi <- matrix(
#'   data = c(
#'     -0.357, 0.771, -0.450,
#'     0.0, -0.511, 0.729,
#'     0, 0, -0.693
#'   ),
#'   nrow = 3
#' )
#' colnames(phi) <- rownames(phi) <- c("x", "m", "y")
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(med)
#'
#' # ---------------------------------------------------------------------------
#' # Example 2 -----------------------------------------------------------------
#' # ---------------------------------------------------------------------------
#' phi <- matrix(
#'   data = c(
#'     -6, 5.5, 0, 0,
#'     1.25, -2.5, 5.9, -7.3,
#'     0, 0, -6, 2.5,
#'     5, 0, 0, -6
#'   ),
#'   nrow = 4
#' )
#' colnames(phi) <- rownames(phi) <- paste0("y", 1:4)
#'
#' # Range of time-intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = seq(from = 0, to = 5, length.out = 500),
#'   from = "y2",
#'   to = "y4",
#'   med = c("y1", "y3")
#' )
#' plot(med)
#'
#' @keywords methods
#' @export
plot.ctmedmed <- function(x,
                          ...) {
  .PlotMed(
    object = x
  )
}

#' Plot Method for an Object of Class `ctmeddelta`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmeddelta`.
#' @param alpha Numeric.
#'   Significance level
#' @param ... Additional arguments.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Range of time-intervals ---------------------------------------------------
#' delta <- DeltaMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
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
                            ...) {
  .PlotDeltaMed(
    object = x,
    alpha = alpha
  )
}

#' Plot Method for an Object of Class `ctmedmc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmedmc`.
#' @param alpha Numeric.
#'   Significance level
#' @param ... Additional arguments.
#'
#' @examples
#' data("deboeck2015phi", package = "cTMed")
#' phi <- deboeck2015phi$dynr$phi
#' vcov_phi_vec <- deboeck2015phi$dynr$vcov
#'
#' # Range of time-intervals ---------------------------------------------------
#' mc <- MCMed(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:20,
#'   from = "x",
#'   to = "y",
#'   med = "m",
#'   R = 5 # use a large value for R in actual research
#' )
#' plot(mc)
#'
#' @keywords methods
#' @export
plot.ctmedmc <- function(x,
                         alpha = 0.05,
                         ...) {
  .PlotMCMed(
    object = x,
    alpha = alpha
  )
}

#' Print Method for Object of Class `ctmedboot`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedboot`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param digits Integer indicating the number of decimal places to display.
#' @param type Charater string.
#'   Confidence interval type, that is,
#'   `type = "pc"` for percentile;
#'   `type = "bc"` for bias corrected.
#' @param ... further arguments.
#'
#' @return Prints a list of matrices of
#'   time intervals,
#'   estimates,
#'   standard errors,
#'   number of bootstrap replications,
#'   and
#'   confidence intervals.
#'
#' @examples
#' \dontrun{
#' library(simStateSpace)
#' # prepare parameters
#' ## number of individuals
#' n <- 50
#' ## time points
#' time <- 100
#' delta_t <- 0.10
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- matrix(
#'   data = c(
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0
#'   ),
#'   nrow = p
#' )
#' sigma0_l <- t(chol(sigma0))
#' mu <- rep(x = 0, times = p)
#' phi <- matrix(
#'   data = c(
#'     -0.357,
#'     0.771,
#'     -0.450,
#'     0.0,
#'     -0.511,
#'     0.729,
#'     0,
#'     0,
#'     -0.693
#'   ),
#'   nrow = p
#' )
#' sigma <- matrix(
#'   data = c(
#'     0.24455556,
#'     0.02201587,
#'     -0.05004762,
#'     0.02201587,
#'     0.07067800,
#'     0.01539456,
#'     -0.05004762,
#'     0.01539456,
#'     0.07553061
#'   ),
#'   nrow = p
#' )
#' sigma_l <- t(chol(sigma))
#' ## measurement model
#' k <- 3
#' nu <- rep(x = 0, times = k)
#' lambda <- diag(k)
#' theta <- 0.2 * diag(k)
#' theta_l <- t(chol(theta))
#'
#' boot <- PBSSMOUFixed(
#'   R = 1000L,
#'   path = getwd(),
#'   prefix = "ou",
#'   n = n,
#'   time = time,
#'   delta_t = delta_t,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   mu = mu,
#'   phi = phi,
#'   sigma_l = sigma_l,
#'   nu = nu,
#'   lambda = lambda,
#'   theta_l = theta_l,
#'   ncores = parallel::detectCores() - 1,
#'   seed = 42
#' )
#' phi_hat <- phi
#' colnames(phi_hat) <- rownames(phi_hat) <- c("x", "m", "y")
#' phi <- extract(object = boot, what = "phi")
#'
#' # Specific time interval ----------------------------------------------------
#' boot <- BootMed(
#'   phi = phi,
#'   phi_hat = phi_hat,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(boot)
#' print(boot, type = "bc") # bias-corrected
#'
#' # Range of time intervals ---------------------------------------------------
#' boot <- BootMed(
#'   phi = phi,
#'   phi_hat = phi_hat,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(boot)
#' print(boot, type = "bc") # bias-corrected
#' }
#'
#' @keywords methods
#' @export
print.ctmedboot <- function(x,
                            alpha = 0.05,
                            digits = 4,
                            type = "pc",
                            ...) {
  if (x$args$network) {
    if (x$args$total) {
      cat(
        paste0(
          "\nTotal Effect Centrality\n",
          "type = ", type, "\n"
        )
      )
    } else {
      cat(
        paste0(
          "\nIndirect Effect Centrality\n",
          "type = ", type, "\n"
        )
      )
    }
  } else {
    cat(
      paste0(
        "\nTotal, Direct, and Indirect Effects\n",
        "type = ", type, "\n"
      )
    )
  }
  base::print(
    lapply(
      X = .BootCI(
        object = x,
        alpha = alpha,
        type = type
      ),
      FUN = round,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `ctmedboot`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `ctmedboot`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#' @param type Charater string.
#'   Confidence interval type, that is,
#'   `type = "pc"` for percentile;
#'   `type = "bc"` for bias corrected.
#'
#' @return Returns a data frame of
#'   effects,
#'   time intervals,
#'   estimates,
#'   standard errors,
#'   number of bootstrap replications,
#'   and
#'   confidence intervals.
#'
#' @examples
#' \dontrun{
#' library(simStateSpace)
#' # prepare parameters
#' ## number of individuals
#' n <- 50
#' ## time points
#' time <- 100
#' delta_t <- 0.10
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- matrix(
#'   data = c(
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0
#'   ),
#'   nrow = p
#' )
#' sigma0_l <- t(chol(sigma0))
#' mu <- rep(x = 0, times = p)
#' phi <- matrix(
#'   data = c(
#'     -0.357,
#'     0.771,
#'     -0.450,
#'     0.0,
#'     -0.511,
#'     0.729,
#'     0,
#'     0,
#'     -0.693
#'   ),
#'   nrow = p
#' )
#' sigma <- matrix(
#'   data = c(
#'     0.24455556,
#'     0.02201587,
#'     -0.05004762,
#'     0.02201587,
#'     0.07067800,
#'     0.01539456,
#'     -0.05004762,
#'     0.01539456,
#'     0.07553061
#'   ),
#'   nrow = p
#' )
#' sigma_l <- t(chol(sigma))
#' ## measurement model
#' k <- 3
#' nu <- rep(x = 0, times = k)
#' lambda <- diag(k)
#' theta <- 0.2 * diag(k)
#' theta_l <- t(chol(theta))
#'
#' boot <- PBSSMOUFixed(
#'   R = 1000L,
#'   path = getwd(),
#'   prefix = "ou",
#'   n = n,
#'   time = time,
#'   delta_t = delta_t,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   mu = mu,
#'   phi = phi,
#'   sigma_l = sigma_l,
#'   nu = nu,
#'   lambda = lambda,
#'   theta_l = theta_l,
#'   ncores = parallel::detectCores() - 1,
#'   seed = 42
#' )
#' phi_hat <- phi
#' colnames(phi_hat) <- rownames(phi_hat) <- c("x", "m", "y")
#' phi <- extract(object = boot, what = "phi")
#'
#' # Specific time interval ----------------------------------------------------
#' boot <- BootMed(
#'   phi = phi,
#'   phi_hat = phi_hat,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' summary(boot)
#' summary(boot, type = "bc") # bias-corrected
#'
#' # Range of time intervals ---------------------------------------------------
#' boot <- BootMed(
#'   phi = phi,
#'   phi_hat = phi_hat,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' summary(boot)
#' summary(boot, type = "bc") # bias-corrected
#' }
#'
#' @keywords methods
#' @export
summary.ctmedboot <- function(object,
                              alpha = 0.05,
                              type = "pc",
                              ...) {
  if (object$args$network) {
    if (object$args$total) {
      if (interactive()) {
        # nocov start
        cat(
          paste0(
            "\nTotal Effect Centrality\n",
            "type = ", type, "\n"
          )
        )
        # nocov end
      }
    } else {
      if (interactive()) {
        # nocov start
        cat(
          paste0(
            "\nIndirect Effect Centrality\n",
            "type = ", type, "\n"
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
          "\nTotal, Direct, and Indirect Effects\n",
          "type = ", type, "\n"
        )
      )
      # nocov end
    }
  }
  ci <- .BootCI(
    object = object,
    alpha = alpha,
    type = type
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
  return(ci[, varnames])
}

#' Bootstrap Method Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `ctmedboot`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#' @param type Charater string.
#'   Confidence interval type, that is,
#'   `type = "pc"` for percentile;
#'   `type = "bc"` for bias corrected.
#'
#' @return Returns a data frame of confidence intervals.
#'
#' @examples
#' \dontrun{
#' library(simStateSpace)
#' # prepare parameters
#' ## number of individuals
#' n <- 50
#' ## time points
#' time <- 100
#' delta_t <- 0.10
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- matrix(
#'   data = c(
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0
#'   ),
#'   nrow = p
#' )
#' sigma0_l <- t(chol(sigma0))
#' mu <- rep(x = 0, times = p)
#' phi <- matrix(
#'   data = c(
#'     -0.357,
#'     0.771,
#'     -0.450,
#'     0.0,
#'     -0.511,
#'     0.729,
#'     0,
#'     0,
#'     -0.693
#'   ),
#'   nrow = p
#' )
#' sigma <- matrix(
#'   data = c(
#'     0.24455556,
#'     0.02201587,
#'     -0.05004762,
#'     0.02201587,
#'     0.07067800,
#'     0.01539456,
#'     -0.05004762,
#'     0.01539456,
#'     0.07553061
#'   ),
#'   nrow = p
#' )
#' sigma_l <- t(chol(sigma))
#' ## measurement model
#' k <- 3
#' nu <- rep(x = 0, times = k)
#' lambda <- diag(k)
#' theta <- 0.2 * diag(k)
#' theta_l <- t(chol(theta))
#'
#' boot <- PBSSMOUFixed(
#'   R = 1000L,
#'   path = getwd(),
#'   prefix = "ou",
#'   n = n,
#'   time = time,
#'   delta_t = delta_t,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   mu = mu,
#'   phi = phi,
#'   sigma_l = sigma_l,
#'   nu = nu,
#'   lambda = lambda,
#'   theta_l = theta_l,
#'   ncores = parallel::detectCores() - 1,
#'   seed = 42
#' )
#' phi_hat <- phi
#' colnames(phi_hat) <- rownames(phi_hat) <- c("x", "m", "y")
#' phi <- extract(object = boot, what = "phi")
#'
#' # Specific time interval ----------------------------------------------------
#' boot <- BootMed(
#'   phi = phi,
#'   phi_hat = phi_hat,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' confint(boot)
#' confint(boot, type = "bc") # bias-corrected
#'
#' # Range of time intervals ---------------------------------------------------
#' boot <- BootMed(
#'   phi = phi,
#'   phi_hat = phi_hat,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' confint(boot)
#' confint(boot, type = "bc") # bias-corrected
#' }
#'
#' @keywords methods
#' @export
confint.ctmedboot <- function(object,
                              parm = NULL,
                              level = 0.95,
                              type = "pc",
                              ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$output[[1]]$est[1:3])
    )
  }
  ci <- .BootCI(
    object = object,
    alpha = 1 - level[1],
    type = type
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
  if (object$args$network) {
    ci$variable <- effect
  } else {
    ci$effect <- effect
  }
  rownames(ci) <- NULL
  varnames <- colnames(ci)
  p <- dim(ci)[2]
  varnames <- varnames[c(p, 1:(p - 1))]
  return(ci[, varnames])
}

#' Plot Method for an Object of Class `ctmedboot`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmedboot`.
#' @param alpha Numeric.
#'   Significance level
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param type Charater string.
#'   Confidence interval type, that is,
#'   `type = "pc"` for percentile;
#'   `type = "bc"` for bias corrected.
#' @param ... Additional arguments.
#'
#' @return Displays plots of point estimates and confidence intervals.
#'
#' @examples
#' \dontrun{
#' library(simStateSpace)
#' # prepare parameters
#' ## number of individuals
#' n <- 50
#' ## time points
#' time <- 100
#' delta_t <- 0.10
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- matrix(
#'   data = c(
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0,
#'     0.2,
#'     0.2,
#'     0.2,
#'     1.0
#'   ),
#'   nrow = p
#' )
#' sigma0_l <- t(chol(sigma0))
#' mu <- rep(x = 0, times = p)
#' phi <- matrix(
#'   data = c(
#'     -0.357,
#'     0.771,
#'     -0.450,
#'     0.0,
#'     -0.511,
#'     0.729,
#'     0,
#'     0,
#'     -0.693
#'   ),
#'   nrow = p
#' )
#' sigma <- matrix(
#'   data = c(
#'     0.24455556,
#'     0.02201587,
#'     -0.05004762,
#'     0.02201587,
#'     0.07067800,
#'     0.01539456,
#'     -0.05004762,
#'     0.01539456,
#'     0.07553061
#'   ),
#'   nrow = p
#' )
#' sigma_l <- t(chol(sigma))
#' ## measurement model
#' k <- 3
#' nu <- rep(x = 0, times = k)
#' lambda <- diag(k)
#' theta <- 0.2 * diag(k)
#' theta_l <- t(chol(theta))
#'
#' boot <- PBSSMOUFixed(
#'   R = 1000L,
#'   path = getwd(),
#'   prefix = "ou",
#'   n = n,
#'   time = time,
#'   delta_t = delta_t,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   mu = mu,
#'   phi = phi,
#'   sigma_l = sigma_l,
#'   nu = nu,
#'   lambda = lambda,
#'   theta_l = theta_l,
#'   ncores = parallel::detectCores() - 1,
#'   seed = 42
#' )
#' phi_hat <- phi
#' colnames(phi_hat) <- rownames(phi_hat) <- c("x", "m", "y")
#' phi <- extract(object = boot, what = "phi")
#'
#' # Range of time intervals ---------------------------------------------------
#' boot <- BootMed(
#'   phi = phi,
#'   phi_hat = phi_hat,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' confint(boot)
#' confint(boot, type = "bc") # bias-corrected
#' }
#'
#' @keywords methods
#' @export
plot.ctmedboot <- function(x,
                           alpha = 0.05,
                           col = NULL,
                           type = "pc",
                           ...) {
  if (x$args$network) {
    return(
      .PlotCentralCI(
        object = x,
        alpha = alpha,
        col = col
      )
    )
  } else {
    if (x$fun == "BootMed" || x$fun == "BootMedStd") {
      return(
        .PlotMedCI(
          object = x,
          alpha = alpha,
          col = col,
          type = type
        )
      )
    }
    if (x$fun == "BootBeta" || x$fun == "BootBetaStd") {
      return(
        .PlotBetaCI(
          object = x,
          alpha = alpha,
          col = col,
          type = type
        )
      )
    }
  }
}

#' Plot Centrality Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of any of the following functions:
#'   [DeltaBeta()],
#'   [MCBeta()], and
#'   [PosteriorBeta()].
#' @param alpha Numeric.
#'   Significance level.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
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
#' # Range of time intervals ---------------------------------------------------
#' beta <- DeltaBeta(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5
#' )
#' plot(beta)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotBetaCI <- function(object,
                        alpha = 0.05,
                        col = NULL) {
  if (length(object$output) == 1) {
    stop(
      paste0(
        "The input argument \'object\' only has a single `delta_t` value.",
        "\n",
        "Not suitable for plotting.",
        "\n"
      )
    )
  }
  stopifnot(length(alpha) == 1)
  stopifnot(
    alpha > 0 && alpha < 1
  )
  if (object$args$method == "mc") {
    mc <- TRUE
    ylab <- "Estimate"
    method <- "Monte Carlo Method"
  }
  if (object$args$method == "posterior") {
    mc <- TRUE
    ylab <- "Posterior"
    method <- "Posterior"
  }
  if (object$args$method == "delta") {
    mc <- FALSE
    ylab <- "Estimate"
    method <- "Delta Method"
  }
  if (mc) {
    ci <- .MCCI(
      object = object,
      alpha = alpha
    )
    ci <- do.call(
      what = "rbind",
      args = ci
    )
    colnames(ci) <- c(
      "interval",
      "est",
      "se",
      "R",
      "ll",
      "ul"
    )
  } else {
    ci <- .DeltaCI(
      object = object,
      alpha = alpha
    )
    ci <- do.call(
      what = "rbind",
      args = ci
    )
    colnames(ci) <- c(
      "interval",
      "est",
      "se",
      "z",
      "p",
      "ll",
      "ul"
    )
  }
  effect <- rownames(ci)
  ci <- as.data.frame(
    ci
  )
  ci$effect <- effect
  rownames(ci) <- NULL
  effect <- unique(
    ci$effect
  )
  if (is.null(col)) {
    col <- grDevices::rainbow(length(effect))
  }
  foo <- function(effect,
                  col,
                  ci) {
    ci <- ci[which(ci$effect == effect), ]
    graphics::plot.default(
      x = 0,
      y = 0,
      xlim = range(ci$interval),
      ylim = range(c(ci$est, ci$ll, ci$ul)),
      type = "n",
      xlab = "Time Interval",
      ylab = ylab,
      main = paste0(
        (1 - alpha) * 100,
        "% CI for the Total Effect ",
        effect,
        " (",
        method,
        ")"
      )
    )
    for (i in seq_along(ci$interval)) {
      if (!(ci$ll[i] <= 0 && 0 <= ci$ul[i])) {
        graphics::segments(
          x0 = ci$interval[i],
          y0 = ci$ll[i],
          x1 = ci$interval[i],
          y1 = ci$ul[i],
          col = col,
          lty = 3,
          lwd = 1
        )
      }
    }
    graphics::abline(
      h = 0
    )
    graphics::lines(
      x = ci$interval,
      y = ci$est,
      type = "l",
      col = col,
      lty = 1,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ll,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ul,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
  }
  for (i in seq_along(effect)) {
    foo(
      effect = effect[i],
      col = col[i],
      ci = ci
    )
  }
}

#' Plot Centrality Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of any of the following functions:
#'   [DeltaTotalCentral()],
#'   [DeltaIndirectCentral()],
#'   [MCTotalCentral()],
#'   [MCIndirectCentral()],
#'   [PosteriorTotalCentral()], and
#'   [PosteriorIndirectCentral()].
#' @param alpha Numeric.
#'   Significance level.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
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
#' # Range of time intervals ---------------------------------------------------
#' total_central <- DeltaTotalCentral(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5
#' )
#' plot(total_central)
#' indirect_central <- DeltaIndirectCentral(
#'   phi = phi,
#'   vcov_phi_vec = vcov_phi_vec,
#'   delta_t = 1:5
#' )
#' plot(indirect_central)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotCentralCI <- function(object,
                           alpha = 0.05,
                           col = NULL) {
  if (length(object$output) == 1) {
    stop(
      paste0(
        "The input argument \'object\' only has a single `delta_t` value.",
        "\n",
        "Not suitable for plotting.",
        "\n"
      )
    )
  }
  stopifnot(length(alpha) == 1)
  stopifnot(
    alpha > 0 && alpha < 1
  )
  if (object$args$method == "mc") {
    mc <- TRUE
    ylab <- "Estimate"
    method <- "Monte Carlo Method"
  }
  if (object$args$method == "posterior") {
    mc <- TRUE
    ylab <- "Posterior"
    method <- "Posterior"
  }
  if (object$args$method == "delta") {
    mc <- FALSE
    ylab <- "Estimate"
    method <- "Delta Method"
  }
  if (mc) {
    ci <- .MCCI(
      object = object,
      alpha = alpha
    )
    ci <- do.call(
      what = "rbind",
      args = ci
    )
    colnames(ci) <- c(
      "interval",
      "est",
      "se",
      "R",
      "ll",
      "ul"
    )
  } else {
    ci <- .DeltaCI(
      object = object,
      alpha = alpha
    )
    ci <- do.call(
      what = "rbind",
      args = ci
    )
    colnames(ci) <- c(
      "interval",
      "est",
      "se",
      "z",
      "p",
      "ll",
      "ul"
    )
  }
  effect <- rownames(ci)
  ci <- as.data.frame(
    ci
  )
  ci$effect <- effect
  rownames(ci) <- NULL
  effect <- unique(
    ci$effect
  )
  if (is.null(col)) {
    col <- grDevices::rainbow(length(effect))
  }
  if (object$args$total) {
    centrality <- " Total Effect Centrality of "
  } else {
    centrality <- " Indirect Effect Centrality of "
  }
  foo <- function(effect,
                  col,
                  ci) {
    ci <- ci[which(ci$effect == effect), ]
    if (object$fun == "DeltaBeta") {
      main <- paste0(
        (1 - alpha) * 100,
        "% CI for",
        effect,
        " (",
        method,
        ")"
      )
    } else {
      main <- paste0(
        (1 - alpha) * 100,
        "% CI for the",
        centrality,
        effect,
        " (",
        method,
        ")"
      )
    }
    graphics::plot.default(
      x = 0,
      y = 0,
      xlim = range(ci$interval),
      ylim = range(c(ci$est, ci$ll, ci$ul)),
      type = "n",
      xlab = "Time Interval",
      ylab = ylab,
      main = main
    )
    for (i in seq_along(ci$interval)) {
      if (!(ci$ll[i] <= 0 && 0 <= ci$ul[i])) {
        graphics::segments(
          x0 = ci$interval[i],
          y0 = ci$ll[i],
          x1 = ci$interval[i],
          y1 = ci$ul[i],
          col = col,
          lty = 3,
          lwd = 1
        )
      }
    }
    graphics::abline(
      h = 0
    )
    graphics::lines(
      x = ci$interval,
      y = ci$est,
      type = "l",
      col = col,
      lty = 1,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ll,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ul,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
  }
  for (i in seq_along(effect)) {
    foo(
      effect = effect[i],
      col = col[i],
      ci = ci
    )
  }
}

#' Plot Results of The TotalCentral
#' or The IndirectCentral Functions
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [TotalCentral()] or the [IndirectCentral()] functions.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
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
#'
#' # Range of time intervals ---------------------------------------------------
#' total_central <- TotalCentral(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' plot(total_central)
#' indirect_central <- IndirectCentral(
#'   phi = phi,
#'   delta_t = 1:5
#' )
#' plot(indirect_central)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotCentral <- function(object,
                         col = NULL,
                         legend_pos = "topright") {
  if (dim(object$output)[1] == 1) {
    stop(
      paste0(
        "The input argument \'object\' only has a single `delta_t` value.",
        "\n",
        "Not suitable for plotting.",
        "\n"
      )
    )
  }
  if (object$args$total) {
    main <- "Total Effect Centrality"
  } else {
    main <- "Indirect Effect Centrality"
  }
  delta_t <- object$output[, "interval"]
  varnames <- colnames(
    object$args$phi
  )
  if (is.null(col)) {
    col <- grDevices::rainbow(length(varnames))
  }
  graphics::plot.default(
    x = 0,
    y = 0,
    xlim = range(delta_t),
    ylim = range(
      object$output[
        ,
        varnames
      ]
    ),
    type = "n",
    xlab = "Time Interval",
    ylab = "Parameter Value",
    main = main
  )
  graphics::abline(
    h = 0
  )
  for (i in seq_along(varnames)) {
    graphics::lines(
      x = delta_t,
      y = object$output[
        ,
        varnames[i]
      ],
      type = "l",
      col = col[i],
      lty = i,
      lwd = 2
    )
  }
  graphics::legend(
    x = legend_pos,
    legend = varnames,
    lty = seq_len(length(varnames)),
    col = col,
    cex = 0.8,
    lwd = 2
  )
}

#' Plot Total, Direct, and Indirect Effects Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [DeltaMed()], [MCMed()], [PosteriorMed()] functions.
#' @param alpha Numeric.
#'   Significance level.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
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
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotMedCI <- function(object,
                       alpha = 0.05,
                       col = NULL) {
  if (length(object$output) == 1) {
    stop(
      paste0(
        "The input argument \'object\' only has a single `delta_t` value.",
        "\n",
        "Not suitable for plotting.",
        "\n"
      )
    )
  }
  stopifnot(length(alpha) == 1)
  stopifnot(
    alpha > 0 && alpha < 1
  )
  if (object$args$method == "mc") {
    mc <- TRUE
    ylab <- "Estimate"
    method <- "Monte Carlo Method"
  }
  if (object$args$method == "posterior") {
    mc <- TRUE
    ylab <- "Posterior"
    method <- "Posterior"
  }
  if (object$args$method == "delta") {
    mc <- FALSE
    ylab <- "Estimate"
    method <- "Delta Method"
  }
  if (mc) {
    ci <- .MCCI(
      object = object,
      alpha = alpha
    )
    ci <- do.call(
      what = "rbind",
      args = ci
    )
    colnames(ci) <- c(
      "interval",
      "est",
      "se",
      "R",
      "ll",
      "ul"
    )
  } else {
    ci <- .DeltaCI(
      object = object,
      alpha = alpha
    )
    ci <- do.call(
      what = "rbind",
      args = ci
    )
    colnames(ci) <- c(
      "interval",
      "est",
      "se",
      "z",
      "p",
      "ll",
      "ul"
    )
  }
  effect <- rownames(ci)
  ci <- as.data.frame(
    ci
  )
  ci$effect <- effect
  rownames(ci) <- NULL
  effect <- unique(
    ci$effect
  )
  if (is.null(col)) {
    col <- c(
      "#5e3c99",
      "#2c7bb6",
      "#d7191c"
    )
  }
  foo <- function(effect,
                  col,
                  ci) {
    ci <- ci[which(ci$effect == effect), ]
    graphics::plot.default(
      x = 0,
      y = 0,
      xlim = range(ci$interval),
      ylim = range(c(ci$est, ci$ll, ci$ul)),
      type = "n",
      xlab = "Time Interval",
      ylab = ylab,
      main = paste0(
        (1 - alpha) * 100,
        "% CI for the ",
        gsub(
          pattern = "(^|[[:space:]])([[:alpha:]])",
          replacement = "\\1\\U\\2",
          x = effect,
          perl = TRUE
        ),
        " Effect (",
        method,
        ")"
      )
    )
    for (i in seq_along(ci$interval)) {
      if (!(ci$ll[i] <= 0 && 0 <= ci$ul[i])) {
        graphics::segments(
          x0 = ci$interval[i],
          y0 = ci$ll[i],
          x1 = ci$interval[i],
          y1 = ci$ul[i],
          col = col,
          lty = 3,
          lwd = 1
        )
      }
    }
    graphics::abline(
      h = 0
    )
    graphics::lines(
      x = ci$interval,
      y = ci$est,
      type = "l",
      col = col,
      lty = 1,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ll,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
    graphics::lines(
      x = ci$interval,
      y = ci$ul,
      type = "l",
      col = col,
      lty = 3,
      lwd = 2
    )
  }
  for (i in seq_along(effect)) {
    foo(
      effect = effect[i],
      col = col[i],
      ci = ci
    )
  }
}

#' Plot Results of The Med Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [Med()] function.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
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
#'
#' # Range of time intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' plot(med)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotMed <- function(object,
                     col = NULL,
                     legend_pos = "topright") {
  if (dim(object$output)[1] == 1) {
    stop(
      paste0(
        "The input argument \'object\' only has a single `delta_t` value.",
        "\n",
        "Not suitable for plotting.",
        "\n"
      )
    )
  }
  if (is.null(col)) {
    col_direct <- "#2c7bb6"
    col_indirect <- "#d7191c"
    col_total <- "#5e3c99"
  } else {
    col_direct <- col[1]
    col_indirect <- col[2]
    col_total <- col[3]
  }
  delta_t <- object$output[, "interval"]
  graphics::plot.default(
    x = 0,
    y = 0,
    xlim = range(delta_t),
    ylim = range(
      object$output[
        ,
        c(
          "total",
          "direct",
          "indirect"
        )
      ]
    ),
    type = "n",
    xlab = "Time Interval",
    ylab = "Parameter Value",
    main = "Total, Direct, and Indirect Effects"
  )
  graphics::abline(
    h = 0
  )
  graphics::lines(
    x = delta_t,
    y = object$output[
      ,
      "indirect"
    ],
    type = "l",
    col = col_indirect,
    lty = 1,
    lwd = 2
  )
  graphics::lines(
    x = delta_t,
    y = object$output[
      ,
      "direct"
    ],
    type = "l",
    col = col_direct,
    lty = 2,
    lwd = 2
  )
  graphics::lines(
    x = delta_t,
    y = object$output[
      ,
      "total"
    ],
    type = "l",
    col = col_total,
    lty = 3,
    lwd = 2
  )
  graphics::legend(
    x = legend_pos,
    legend = c("Indirect", "Direct", "Total"),
    lty = c(1, 2, 3),
    col = c(col_indirect, col_direct, col_total),
    cex = 0.8,
    lwd = 2
  )
}

#' Plot Results of The Trajectory Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object R object.
#'   Output of the [Med()] function.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
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
#'
#' traj <- Trajectory(
#'   mu0 = c(3, 3, -3),
#'   time = 150,
#'   phi = phi,
#'   med = "m"
#' )
#'
#' plot(traj)
#'
#' @family Continuous Time Mediation Functions
#' @keywords cTMed plot
#' @noRd
.PlotTrajectory <- function(object,
                            legend_pos = "topright") {
  idx <- rownames(object$args$phi)
  p <- length(idx)
  ylab <- idx
  varnames <- paste0("y", seq_len(p))
  phi <- simStateSpace:::as.data.frame.simstatespace(
    object$output$total
  )
  phi_direct <- simStateSpace:::as.data.frame.simstatespace(
    object$output$direct
  )
  phi_indirect <- simStateSpace:::as.data.frame.simstatespace(
    object$output$indirect
  )
  time <- phi[, "time"]
  phi_vec <- phi[, varnames]
  dim(phi_vec) <- NULL
  phi_direct_vec <- phi_direct[, varnames]
  dim(phi_direct_vec) <- NULL
  phi_indirect_vec <- phi_indirect[, varnames]
  dim(phi_indirect_vec) <- NULL
  phi <- phi[, varnames]
  phi_direct <- phi_direct[, varnames]
  phi_indirect <- phi_indirect[, varnames]
  col_direct <- "#2c7bb6"
  col_indirect <- "#d7191c"
  col_total <- "#5e3c99"
  for (i in seq_len(p)) {
    graphics::plot.default(
      x = 0,
      y = 0,
      xlim = range(time),
      ylim = range(c(phi_vec, phi_direct_vec, phi_indirect_vec)),
      type = "n",
      xlab = "Time",
      ylab = ylab[i],
      main = ""
    )
    graphics::abline(
      h = 0
    )
    graphics::lines(
      x = time,
      y = phi[, i],
      type = "l",
      col = col_total,
      lty = 3,
      lwd = 2
    )
    graphics::lines(
      x = time,
      y = phi_direct[, i],
      type = "l",
      col = col_direct,
      lty = 2,
      lwd = 2
    )
    graphics::lines(
      x = time,
      y = phi_indirect[, i],
      type = "l",
      col = col_indirect,
      lty = 1,
      lwd = 2
    )
    graphics::legend(
      x = legend_pos,
      legend = c("Indirect", "Direct", "Total"),
      lty = 1:3,
      col = c(col_indirect, col_direct, col_total),
      cex = 0.8,
      lwd = 2
    )
  }
}

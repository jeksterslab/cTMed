#' Print Method for Object of Class `ctmedmed`
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param x an object of class `ctmedmed`.
#' @param digits Integer indicating the number of decimal places to display.
#' @param ... further arguments.
#'
#' @return Prints a matrix of effects.
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
#' # Specific time interval ----------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(med)
#'
#' # Range of time intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' print(med)
#'
#' @keywords methods
#' @export
print.ctmedmed <- function(x,
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
  varnames <- colnames(x$output)
  varnames <- c(
    "interval",
    varnames[which(varnames != "interval")]
  )
  base::print(
    round(
      x$output[
        ,
        varnames,
        drop = FALSE
      ],
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
#'
#' @return Returns a matrix of effects.
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
#' # Specific time interval ----------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' summary(med)
#'
#' # Range of time intervals ---------------------------------------------------
#' med <- Med(
#'   phi = phi,
#'   delta_t = 1:5,
#'   from = "x",
#'   to = "y",
#'   med = "m"
#' )
#' summary(med)
#'
#' @keywords methods
#' @export
summary.ctmedmed <- function(object,
                             digits = 4,
                             ...) {
  if (object$args$network) {
    out <- round(
      object$output[
        ,
        c(
          "interval",
          colnames(object$args$phi)
        ),
        drop = FALSE
      ],
      digits = digits
    )
  } else {
    out <- round(
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
  }
  out
}

#' Plot Method for an Object of Class `ctmedmed`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `ctmedmed`.
#' @param col Character vector.
#'   Optional argument.
#'   Character vector of colors.
#' @param legend_pos Character vector.
#'   Optional argument.
#'   Legend position.
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
#' @keywords methods
#' @export
plot.ctmedmed <- function(x,
                          col = NULL,
                          legend_pos = "topright",
                          ...) {
  if (x$args$network) {
    out <- .PlotCentral(
      object = x,
      col = col,
      legend_pos = legend_pos
    )
  } else {
    out <- .PlotMed(
      object = x,
      col = col,
      legend_pos = legend_pos
    )
  }
  out
}

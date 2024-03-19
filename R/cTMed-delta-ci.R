.DeltaCI <- function(object,
                     alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    all(alpha > 0 & alpha < 1)
  )
  return(
    lapply(
      X = object$output,
      FUN = function(i) {
        ci <- .CIWald(
          est = i$est,
          se = sqrt(
            diag(
              i$vcov
            )
          ),
          theta = 0,
          alpha = alpha,
          z = TRUE,
          test = FALSE
        )
        ci <- cbind(
          interval = i$delta_t,
          ci
        )
        rownames(ci) <- c(
          "total",
          "direct",
          "indirect"
        )
        return(ci)
      }
    )
  )
}

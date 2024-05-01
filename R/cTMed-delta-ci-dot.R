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
        if (object$args$network) {
          rownames(ci) <- colnames(
            object$args$phi
          )
        } else {
          if (object$fun == "DeltaMed") {
            rownames(ci) <- c(
              "total",
              "direct",
              "indirect"
            )
          }
          if (object$fun == "DeltaBeta") {
            varnames <- colnames(
              object$args$phi
            )
            x <- expand.grid(
              to = varnames,
              from = varnames
            )
            rownames(ci) <- sapply(
              X = seq_len(dim(x)[1]),
              FUN = function(i) {
                paste0("from ", x[i, 2], " to ", x[i, 1])
              }
            )
          }
        }
        return(ci)
      }
    )
  )
}

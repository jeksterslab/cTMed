.MCCI <- function(object,
                  alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    all(alpha > 0 & alpha < 1)
  )
  return(
    lapply(
      X = object$output,
      FUN = function(x) {
        thetahat <- x$est[1:3]
        thetahatstar <- x$thetahatstar[, 1:3, drop = FALSE]
        probs <- .PCProbs(alpha = alpha)
        ci <- vector(
          mode = "list",
          length = 3
        )
        for (i in seq_len(3)) {
          ci[[i]] <- .PCCI(
            thetahatstar = thetahatstar[, i],
            thetahat = thetahat[i],
            probs = probs
          )
        }
        ci <- do.call(
          what = "rbind",
          args = ci
        )
        ci <- cbind(
          interval = x$delta_t,
          ci
        )
        rownames(ci) <- colnames(thetahatstar)
        return(ci)
      }
    )
  )
}

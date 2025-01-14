.BootCI <- function(object,
                    alpha = c(0.05, 0.01, 0.001),
                    type = "pc") {
  return(
    lapply(
      X = object$output,
      FUN = function(x) {
        idx <- seq_len(length(x$est) - 1)
        thetahat <- x$est[idx]
        thetahatstar <- x$thetahatstar[, idx, drop = FALSE]
        probs <- .PCProbs(alpha = alpha)
        ci <- vector(
          mode = "list",
          length = length(idx)
        )
        for (i in idx) {
          if (type == "pc") {
            thetahatstar_i <- as.vector(
              thetahatstar[, i]
            )
            thetahatstar_i <- thetahatstar[
              stats::complete.cases(thetahatstar_i)
            ]
            ci[[i]] <- .PCCI(
              thetahatstar = thetahatstar_i,
              thetahat = thetahat[i],
              probs = probs
            )
          }
          if (type == "bc") {
            thetahatstar_i <- as.vector(
              thetahatstar[, i]
            )
            thetahatstar_i <- thetahatstar[
              stats::complete.cases(thetahatstar_i)
            ]
            ci[[i]] <- .BCCI(
              thetahatstar = thetahatstar_i,
              thetahat = thetahat[i],
              probs = probs,
              z0 = .Z0(
                thetahatstar = thetahatstar_i,
                thetahat = thetahat[i]
              ),
              z1 = .Z1(
                probs = probs
              )
            )
          }
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

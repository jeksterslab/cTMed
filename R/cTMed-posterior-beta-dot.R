.PosteriorBeta <- function(phi,
                           delta_t,
                           ncores = NULL) {
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  varnames <- colnames(phi[[1]])
  p <- dim(phi[[1]])[1]
  x <- do.call(
    what = "rbind",
    args = lapply(
      X = phi,
      FUN = function(x) {
        dim(x) <- NULL
        return(x)
      }
    )
  )
  phi_vec <- colMeans(x)
  phi_mean <- matrix(
    data = phi_vec,
    nrow = p
  )
  colnames(phi_mean) <- rownames(phi_mean) <- varnames
  x <- expand.grid(
    to = varnames,
    from = varnames
  )
  varnames <- c(
    sapply(
      X = seq_len(dim(x)[1]),
      FUN = function(i) {
        paste0("from ", x[i, 2], " to ", x[i, 1])
      }
    ),
    "interval"
  )
  if (par) {
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    output <- lapply(
      X = delta_t,
      FUN = function(i) {
        thetahatstar <- parallel::parLapply(
          cl = cl,
          X = phi,
          fun = .TotalDeltaT,
          delta_t = i
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
        colnames(thetahatstar) <- varnames
        est <- .TotalDeltaT(
          phi = phi_mean,
          delta_t = i
        )
        names(est) <- varnames
        out <- list(
          delta_t = i,
          est = est,
          thetahatstar = thetahatstar
        )
        return(out)
      }
    )
  } else {
    output <- lapply(
      X = delta_t,
      FUN = function(i) {
        thetahatstar <- lapply(
          X = phi,
          FUN = .TotalDeltaT,
          delta_t = i
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
        colnames(thetahatstar) <- varnames
        est <- .TotalDeltaT(
          phi = phi_mean,
          delta_t = i
        )
        names(est) <- varnames
        out <- list(
          delta_t = i,
          est = est,
          thetahatstar = thetahatstar
        )
        return(out)
      }
    )
  }
  return(output)
}

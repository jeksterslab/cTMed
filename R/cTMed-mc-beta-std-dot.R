.MCBetaStd <- function(phi,
                       sigma,
                       vcov_theta,
                       delta_t,
                       R,
                       test_phi = TRUE,
                       ncores = NULL,
                       seed = NULL) {
  theta <- c(
    .Vec(phi),
    .Vech(sigma)
  )
  varnames <- colnames(phi)
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
  # nocov start
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    # generate phi
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    if (!is.null(seed)) {
      parallel::clusterSetRNGStream(
        cl = cl,
        iseed = seed
      )
    }
    phisigmas <- parallel::parLapply(
      cl = cl,
      X = 1:R,
      fun = function(i) {
        return(
          .MCPhiSigmaI(
            theta = theta,
            vcov_theta = vcov_theta,
            test_phi = test_phi
          )
        )
      }
    )
    output <- lapply(
      X = delta_t,
      FUN = function(i) {
        thetahatstar <- parallel::parLapply(
          cl = cl,
          X = phisigmas,
          fun = function(x,
                         delta_t) {
            return(
              .TotalStdDeltaT(
                phi = x[[1]],
                sigma = x[[2]],
                delta_t = delta_t
              )
            )
          },
          delta_t = i
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
        colnames(thetahatstar) <- varnames
        est <- .TotalStdDeltaT(
          phi = phi,
          sigma = sigma,
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
    # nocov end
  } else {
    # generate phi
    if (!is.null(seed)) {
      set.seed(seed)
    }
    phisigmas <- lapply(
      X = 1:R,
      FUN = function(i) {
        return(
          .MCPhiSigmaI(
            theta = theta,
            vcov_theta = vcov_theta,
            test_phi = test_phi
          )
        )
      }
    )
    output <- lapply(
      X = delta_t,
      FUN = function(i) {
        thetahatstar <- lapply(
          X = phisigmas,
          FUN = function(x,
                         delta_t) {
            return(
              .TotalStdDeltaT(
                phi = x[[1]],
                sigma = x[[2]],
                delta_t = delta_t
              )
            )
          },
          delta_t = i
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
        colnames(thetahatstar) <- varnames
        est <- .TotalStdDeltaT(
          phi = phi,
          sigma = sigma,
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

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
    if (ncores > R) {
      ncores <- R
    }
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    os_type <- Sys.info()["sysname"]
    if (os_type == "Darwin") {
      fork <- TRUE
    } else if (os_type == "Linux") {
      fork <- TRUE
    } else {
      fork <- FALSE
    }
    if (fork) {
      # generate phi
      if (!is.null(seed)) {
        set.seed(seed)
      }
      phisigmas <- parallel::mclapply(
        X = seq_len(R),
        FUN = function(i) {
          .MCPhiSigmaI(
            theta = theta,
            vcov_theta = vcov_theta,
            test_phi = test_phi
          )
        },
        mc.cores = ncores
      )
      output <- lapply(
        X = delta_t,
        FUN = function(i) {
          thetahatstar <- parallel::mclapply(
            X = phisigmas,
            FUN = function(x,
                           delta_t) {
              .TotalStdDeltaT(
                phi = x[[1]],
                sigma = x[[2]],
                delta_t = delta_t
              )
            },
            delta_t = i,
            mc.cores = ncores
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
          list(
            delta_t = i,
            est = est,
            thetahatstar = thetahatstar
          )
        }
      )
    } else {
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
        X = seq_len(R),
        fun = function(i) {
          .MCPhiSigmaI(
            theta = theta,
            vcov_theta = vcov_theta,
            test_phi = test_phi
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
              .TotalStdDeltaT(
                phi = x[[1]],
                sigma = x[[2]],
                delta_t = delta_t
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
          list(
            delta_t = i,
            est = est,
            thetahatstar = thetahatstar
          )
        }
      )
    }
    # nocov end
  } else {
    # generate phi
    if (!is.null(seed)) {
      set.seed(seed)
    }
    phisigmas <- lapply(
      X = seq_len(R),
      FUN = function(i) {
        .MCPhiSigmaI(
          theta = theta,
          vcov_theta = vcov_theta,
          test_phi = test_phi
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
            .TotalStdDeltaT(
              phi = x[[1]],
              sigma = x[[2]],
              delta_t = delta_t
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
        list(
          delta_t = i,
          est = est,
          thetahatstar = thetahatstar
        )
      }
    )
  }
  output
}

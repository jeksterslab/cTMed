.PosteriorBetaStd <- function(phi,
                              sigma,
                              delta_t,
                              ncores = NULL) {
  varnames <- colnames(phi[[1]])
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
  R <- length(phi)
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
      phisigmas <- parallel::mclapply(
        X = seq_len(R),
        FUN = function(i) {
          list(
            phi[[i]],
            sigma[[i]]
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
          list(
            delta_t = i,
            est = colMeans(thetahatstar),
            thetahatstar = thetahatstar
          )
        }
      )
    } else {
      cl <- parallel::makeCluster(ncores)
      on.exit(
        parallel::stopCluster(cl = cl)
      )
      phisigmas <- parallel::parLapply(
        cl = cl,
        X = seq_len(R),
        fun = function(i) {
          list(
            phi[[i]],
            sigma[[i]]
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
          list(
            delta_t = i,
            est = colMeans(thetahatstar),
            thetahatstar = thetahatstar
          )
        }
      )
    }
    # nocov end
  } else {
    phisigmas <- lapply(
      X = seq_len(R),
      FUN = function(i) {
        list(
          phi[[i]],
          sigma[[i]]
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
        list(
          delta_t = i,
          est = colMeans(thetahatstar),
          thetahatstar = thetahatstar
        )
      }
    )
  }
  output
}

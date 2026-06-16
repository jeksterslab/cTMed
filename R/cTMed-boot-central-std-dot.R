.BootCentralStd <- function(phi,
                            sigma,
                            phi_hat,
                            sigma_hat,
                            delta_t,
                            type,
                            ncores = NULL) {
  if (type == "total") {
    Fun <- .TotalCentralStd
  }
  if (type == "direct") {
    Fun <- .DirectCentralStd
  }
  if (type == "indirect") {
    Fun <- .IndirectCentralStd
  }
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
      phis <- parallel::mclapply(
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
            X = phis,
            FUN = function(x,
                           delta_t) {
              Fun(
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
          colnames(thetahatstar) <- colnames(phi_hat)
          thetahatstar <- cbind(
            thetahatstar,
            interval = i
          )
          est <- c(
            Fun(
              phi = phi_hat,
              sigma = sigma_hat,
              delta_t = i
            ),
            i
          )
          names(est) <- c(
            colnames(phi_hat),
            "interval"
          )
          list(
            delta_t = i,
            est = est,
            thetahatstar = thetahatstar
          )
        }
      )
    } else {
      cl <- parallel::makeCluster(ncores)
      on.exit(
        parallel::stopCluster(cl = cl)
      )
      phis <- parallel::parLapply(
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
            X = phis,
            fun = function(x,
                           delta_t) {
              Fun(
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
          colnames(thetahatstar) <- colnames(phi_hat)
          thetahatstar <- cbind(
            thetahatstar,
            interval = i
          )
          est <- c(
            Fun(
              phi = phi_hat,
              sigma = sigma_hat,
              delta_t = i
            ),
            i
          )
          names(est) <- c(
            colnames(phi_hat),
            "interval"
          )
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
    phis <- lapply(
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
          X = phis,
          FUN = function(x,
                         delta_t) {
            Fun(
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
        colnames(thetahatstar) <- colnames(phi_hat)
        thetahatstar <- cbind(
          thetahatstar,
          interval = i
        )
        est <- c(
          Fun(
            phi = phi_hat,
            sigma = sigma_hat,
            delta_t = i
          ),
          i
        )
        names(est) <- c(
          colnames(phi_hat),
          "interval"
        )
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

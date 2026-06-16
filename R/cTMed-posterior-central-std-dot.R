.PosteriorCentralStd <- function(phi,
                                 sigma,
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
              out <- c(
                Fun(
                  phi = x[[1]],
                  sigma = x[[2]],
                  delta_t = delta_t
                ),
                delta_t
              )
              names(out) <- c(
                colnames(x[[1]]),
                "interval"
              )
              out
            },
            delta_t = i,
            mc.cores = ncores
          )
          thetahatstar <- do.call(
            what = "rbind",
            args = thetahatstar
          )
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
              out <- c(
                Fun(
                  phi = x[[1]],
                  sigma = x[[2]],
                  delta_t = delta_t
                ),
                delta_t
              )
              names(out) <- c(
                colnames(x[[1]]),
                "interval"
              )
              out
            },
            delta_t = i
          )
          thetahatstar <- do.call(
            what = "rbind",
            args = thetahatstar
          )
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
            out <- c(
              Fun(
                phi = x[[1]],
                sigma = x[[2]],
                delta_t = delta_t
              ),
              delta_t
            )
            names(out) <- c(
              colnames(x[[1]]),
              "interval"
            )
            out
          },
          delta_t = i
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
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

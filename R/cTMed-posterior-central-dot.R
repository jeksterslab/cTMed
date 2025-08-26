.PosteriorCentral <- function(phi,
                              delta_t,
                              total,
                              ncores = NULL) {
  if (total) {
    Fun <- .TotalCentral
  } else {
    Fun <- .IndirectCentral
  }
  # nocov start
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    R <- length(delta_t)
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
      output <- lapply(
        X = delta_t,
        FUN = function(i) {
          thetahatstar <- parallel::mclapply(
            X = phi,
            FUN = function(phi) {
              out <- c(
                Fun(
                  phi = phi,
                  delta_t = i
                ),
                i
              )
              names(out) <- c(
                colnames(phi),
                "interval"
              )
              out
            },
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
      output <- lapply(
        X = delta_t,
        FUN = function(i) {
          thetahatstar <- parallel::parLapply(
            cl = cl,
            X = phi,
            fun = function(phi) {
              out <- c(
                Fun(
                  phi = phi,
                  delta_t = i
                ),
                i
              )
              names(out) <- c(
                colnames(phi),
                "interval"
              )
              out
            }
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
    output <- lapply(
      X = delta_t,
      FUN = function(i) {
        thetahatstar <- lapply(
          X = phi,
          FUN = function(phi) {
            out <- c(
              Fun(
                phi = phi,
                delta_t = i
              ),
              i
            )
            names(out) <- c(
              colnames(phi),
              "interval"
            )
            out
          }
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

.BootCentral <- function(phi,
                         phi_hat,
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
    R <- length(phi)
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
            FUN = Fun,
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
              delta_t = i
            ),
            i
          )
          names(est) <- c(
            colnames(phi_hat),
            "interval"
          )
          out <- list(
            delta_t = i,
            est = est,
            thetahatstar = thetahatstar
          )
          return(out)
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
            fun = Fun,
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
              delta_t = i
            ),
            i
          )
          names(est) <- c(
            colnames(phi_hat),
            "interval"
          )
          out <- list(
            delta_t = i,
            est = est,
            thetahatstar = thetahatstar
          )
          return(out)
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
          FUN = Fun,
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
            delta_t = i
          ),
          i
        )
        names(est) <- c(
          colnames(phi_hat),
          "interval"
        )
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

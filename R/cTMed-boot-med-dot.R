.BootMed <- function(phi,
                     phi_hat,
                     delta_t,
                     from,
                     to,
                     med,
                     ncores = NULL) {
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
            FUN = .Med,
            delta_t = i,
            from = from,
            to = to,
            med = med,
            mc.cores = ncores
          )
          thetahatstar <- do.call(
            what = "rbind",
            args = thetahatstar
          )
          colnames(thetahatstar) <- c(
            "total",
            "direct",
            "indirect",
            "interval"
          )
          est <- .Med(
            phi = phi_hat,
            delta_t = i,
            from = from,
            to = to,
            med = med
          )
          names(est) <- c(
            "total",
            "direct",
            "indirect",
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
            fun = .Med,
            delta_t = i,
            from = from,
            to = to,
            med = med
          )
          thetahatstar <- do.call(
            what = "rbind",
            args = thetahatstar
          )
          colnames(thetahatstar) <- c(
            "total",
            "direct",
            "indirect",
            "interval"
          )
          est <- .Med(
            phi = phi_hat,
            delta_t = i,
            from = from,
            to = to,
            med = med
          )
          names(est) <- c(
            "total",
            "direct",
            "indirect",
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
          FUN = .Med,
          delta_t = i,
          from = from,
          to = to,
          med = med
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
        colnames(thetahatstar) <- c(
          "total",
          "direct",
          "indirect",
          "interval"
        )
        est <- .Med(
          phi = phi_hat,
          delta_t = i,
          from = from,
          to = to,
          med = med
        )
        names(est) <- c(
          "total",
          "direct",
          "indirect",
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

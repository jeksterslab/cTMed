.MCMedStd <- function(phi,
                      sigma,
                      vcov_theta,
                      delta_t,
                      from,
                      to,
                      med,
                      R,
                      test_phi = TRUE,
                      ncores = NULL,
                      seed = NULL) {
  theta <- c(
    .Vec(phi),
    .Vech(sigma)
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
      phis <- parallel::mclapply(
        X = seq_len(R),
        FUN = function(i) {
          return(
            .MCPhiSigmaI(
              theta = theta,
              vcov_theta = vcov_theta,
              test_phi = test_phi
            )
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
                           delta_t,
                           from,
                           to,
                           med) {
              return(
                .MedStd(
                  phi = x[[1]],
                  sigma = x[[2]],
                  delta_t = delta_t,
                  from = from,
                  to = to,
                  med = med
                )
              )
            },
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
          est <- .MedStd(
            phi = phi,
            sigma = sigma,
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
      phis <- parallel::parLapply(
        cl = cl,
        X = seq_len(R),
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
            X = phis,
            fun = function(x,
                           delta_t,
                           from,
                           to,
                           med) {
              return(
                .MedStd(
                  phi = x[[1]],
                  sigma = x[[2]],
                  delta_t = delta_t,
                  from = from,
                  to = to,
                  med = med
                )
              )
            },
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
          est <- .MedStd(
            phi = phi,
            sigma = sigma,
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
    # generate phi
    if (!is.null(seed)) {
      set.seed(seed)
    }
    phis <- lapply(
      X = seq_len(R),
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
          X = phis,
          FUN = function(x,
                         delta_t,
                         from,
                         to,
                         med) {
            return(
              .MedStd(
                phi = x[[1]],
                sigma = x[[2]],
                delta_t = delta_t,
                from = from,
                to = to,
                med = med
              )
            )
          },
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
        est <- .MedStd(
          phi = phi,
          sigma = sigma,
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

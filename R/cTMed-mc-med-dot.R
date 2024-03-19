.MCMed <- function(phi,
                   vcov_phi_vec,
                   delta_t,
                   from,
                   to,
                   med,
                   R,
                   test_phi = TRUE,
                   ncores = NULL,
                   seed = NULL) {
  tol <- 1e-14
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
    phis <- parallel::parLapply(
      cl = cl,
      X = 1:R,
      fun = function(i) {
        return(
          .MCPhiI(
            phi = phi,
            vcov_phi_vec_l = t(chol(vcov_phi_vec)),
            tol = tol,
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
          phi = phi,
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
    if (!is.null(seed)) {
      set.seed(seed)
    }
    phis <- lapply(
      X = 1:R,
      FUN = function(i) {
        return(
          .MCPhiI(
            phi = phi,
            vcov_phi_vec_l = t(chol(vcov_phi_vec)),
            tol = tol,
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
          phi = phi,
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

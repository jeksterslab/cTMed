.PosteriorMed <- function(phi,
                          delta_t,
                          from,
                          to,
                          med,
                          ncores = NULL) {
  # nocov start
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
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
            out <- .Med(
              phi = phi,
              delta_t = i,
              from = from,
              to = to,
              med = med
            )
            names(out) <- c(
              "total",
              "direct",
              "indirect",
              "interval"
            )
            return(
              out
            )
          }
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
        return(
          list(
            delta_t = i,
            est = colMeans(thetahatstar),
            thetahatstar = thetahatstar
          )
        )
      }
    )
    # nocov end
  } else {
    output <- lapply(
      X = delta_t,
      FUN = function(i) {
        thetahatstar <- lapply(
          X = phi,
          FUN = function(phi) {
            out <- .Med(
              phi = phi,
              delta_t = i,
              from = from,
              to = to,
              med = med
            )
            names(out) <- c(
              "total",
              "direct",
              "indirect",
              "interval"
            )
            return(
              out
            )
          }
        )
        thetahatstar <- do.call(
          what = "rbind",
          args = thetahatstar
        )
        return(
          list(
            delta_t = i,
            est = colMeans(thetahatstar),
            thetahatstar = thetahatstar
          )
        )
      }
    )
  }
  return(output)
}

.PosteriorCentral <- function(phi,
                              delta_t,
                              total,
                              ncores = NULL) {
  if (total) {
    Fun <- .TotalCentral
  } else {
    Fun <- .IndirectCentral
  }
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

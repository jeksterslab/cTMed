.DeltaCentralStd <- function(delta_t,
                             phi,
                             sigma,
                             vcov_theta,
                             sigma_diag,
                             type) {
  if (type == "total") {
    Fun <- .TotalCentralStdVec
  }
  if (type == "direct") {
    Fun <- .DirectCentralStdVec
  }
  if (type == "indirect") {
    Fun <- .IndirectCentralStdVec
  }
  constructor <- function(delta_t,
                          varnames,
                          sigma_diag) {
    function(x) {
      output <- Fun(
        v = x,
        delta_t = delta_t,
        sigma_diag = sigma_diag
      )
      names(output) <- varnames
      output
    }
  }
  func <- constructor(
    delta_t = delta_t,
    varnames = colnames(phi),
    sigma_diag = sigma_diag
  )
  if (sigma_diag) {
    v <- c(
      .Vec(phi),
      diag(sigma)
    )
  } else {
    v <- c(
      .Vec(phi),
      .Vech(sigma)
    )
  }
  jacobian <- numDeriv::jacobian(
    func = func,
    x = v
  )
  list(
    delta_t = delta_t,
    jacobian = jacobian,
    est = func(x = v),
    vcov = jacobian %*% vcov_theta %*% t(jacobian)
  )
}

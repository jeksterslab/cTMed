.DeltaBetaStd <- function(delta_t,
                          phi,
                          sigma,
                          vcov_theta,
                          sigma_diag) {
  constructor <- function(delta_t,
                          sigma_diag) {
    function(x) {
      .TotalStdVec(
        v = x,
        delta_t = delta_t,
        sigma_diag = sigma_diag
      )
    }
  }
  func <- constructor(
    delta_t = delta_t,
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

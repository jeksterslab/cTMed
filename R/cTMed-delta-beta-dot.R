.DeltaBeta <- function(delta_t,
                       phi,
                       vcov_phi_vec) {
  constructor <- function(delta_t) {
    function(x) {
      .TotalVec(
        phi_vec = x,
        delta_t = delta_t
      )
    }
  }
  func <- constructor(
    delta_t = delta_t
  )
  dim(phi) <- NULL
  jacobian <- numDeriv::jacobian(
    func = func,
    x = phi
  )
  list(
    delta_t = delta_t,
    jacobian = jacobian,
    est = func(x = phi),
    vcov = jacobian %*% vcov_phi_vec %*% t(jacobian)
  )
}

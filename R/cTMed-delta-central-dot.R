.DeltaCentral <- function(delta_t,
                          phi,
                          vcov_phi_vec,
                          centrality) {
  if (centrality == "total") {
    Fun <- .TotalCentralVec
  }
  if (centrality == "direct") {
    Fun <- .DirectCentralVec
  }
  if (centrality == "indirect") {
    Fun <- .IndirectCentralVec
  }
  constructor <- function(delta_t,
                          varnames) {
    function(x) {
      output <- Fun(
        phi_vec = x,
        delta_t = delta_t
      )
      names(output) <- varnames
      output
    }
  }
  func <- constructor(
    delta_t = delta_t,
    varnames = colnames(phi)
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

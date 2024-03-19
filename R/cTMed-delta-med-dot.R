.DeltaMed <- function(delta_t,
                      phi,
                      vcov_phi_vec,
                      p,
                      from,
                      to,
                      med) {
  constructor <- function(delta_t,
                          p,
                          from,
                          to,
                          med) {
    return(
      function(x) {
        return(
          .MedVec(
            phi_vec = x,
            delta_t = delta_t,
            from = from,
            to = to,
            med = med
          )
        )
      }
    )
  }
  func <- constructor(
    delta_t = delta_t,
    p = p,
    from = from,
    to = to,
    med = med
  )
  dim(phi) <- NULL
  jacobian <- numDeriv::jacobian(
    func = func,
    x = phi
  )
  return(
    list(
      delta_t = delta_t,
      jacobian = jacobian,
      est = func(x = phi),
      vcov = jacobian %*% vcov_phi_vec %*% t(jacobian)
    )
  )
}

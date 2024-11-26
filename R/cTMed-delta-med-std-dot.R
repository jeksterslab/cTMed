.DeltaMedStd <- function(delta_t,
                         phi,
                         sigma,
                         vcov_theta,
                         from,
                         to,
                         med) {
  constructor <- function(delta_t,
                          from,
                          to,
                          med) {
    return(
      function(x) {
        return(
          .MedStdVec(
            v = x,
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
    from = from,
    to = to,
    med = med
  )
  v <- c(
    .Vec(phi),
    .Vech(sigma)
  )
  jacobian <- numDeriv::jacobian(
    func = func,
    x = v
  )
  return(
    list(
      delta_t = delta_t,
      jacobian = jacobian,
      est = func(x = v),
      vcov = jacobian %*% vcov_theta %*% t(jacobian)
    )
  )
}

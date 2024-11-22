.DeltaMedStd <- function(delta_t,
                         phi,
                         vcov_phi_vec,
                         sigma,
                         vcov_sigma_vech,
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
  p <- dim(phi)[1]
  q <- (p * (p + 1)) / 2
  vcov_v <- rbind(
    cbind(
      vcov_phi_vec,
      matrix(
        data = 0,
        nrow = p * p,
        ncol = q
      )
    ),
    cbind(
      matrix(
        data = 0,
        nrow = q,
        ncol = p * p
      ),
      vcov_sigma_vech
    )
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
      vcov = jacobian %*% vcov_v %*% t(jacobian)
    )
  )
}

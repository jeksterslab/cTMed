## ---- test-cTMed-delta-direct-central
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "DeltaDirectCentral"),
      {
        testthat::skip_on_cran()
        phi <- matrix(
          data = c(
            -0.357, 0.771, -0.450,
            0.0, -0.511, 0.729,
            0, 0, -0.693
          ),
          nrow = 3
        )
        colnames(phi) <- rownames(phi) <- c("x", "m", "y")
        vcov_phi_vec <- matrix(
          data = c(
            0.00843, 0.00040, -0.00151,
            -0.00600, -0.00033, 0.00110,
            0.00324, 0.00020, -0.00061,
            0.00040, 0.00374, 0.00016,
            -0.00022, -0.00273, -0.00016,
            0.00009, 0.00150, 0.00012,
            -0.00151, 0.00016, 0.00389,
            0.00103, -0.00007, -0.00283,
            -0.00050, 0.00000, 0.00156,
            -0.00600, -0.00022, 0.00103,
            0.00644, 0.00031, -0.00119,
            -0.00374, -0.00021, 0.00070,
            -0.00033, -0.00273, -0.00007,
            0.00031, 0.00287, 0.00013,
            -0.00014, -0.00170, -0.00012,
            0.00110, -0.00016, -0.00283,
            -0.00119, 0.00013, 0.00297,
            0.00063, -0.00004, -0.00177,
            0.00324, 0.00009, -0.00050,
            -0.00374, -0.00014, 0.00063,
            0.00495, 0.00024, -0.00093,
            0.00020, 0.00150, 0.00000,
            -0.00021, -0.00170, -0.00004,
            0.00024, 0.00214, 0.00012,
            -0.00061, 0.00012, 0.00156,
            0.00070, -0.00012, -0.00177,
            -0.00093, 0.00012, 0.00223
          ),
          nrow = 9
        )
        delta_t <- 2
        answer <- rep(
          x = 0.0,
          times = nrow(phi)
        )
        for (m in seq_len(nrow(phi))) {
          d <- diag(nrow(phi))
          d[m, m] <- 0
          direct <- expm::expm(
            delta_t * d %*% phi %*% d
          )
          for (j in seq_len(nrow(phi))) {
            for (i in seq_len(nrow(phi))) {
              if (!(m == i || m == j || i == j)) {
                answer[m] <- answer[m] + direct[i, j]
              }
            }
          }
        }
        delta <- DeltaDirectCentral(
          phi = phi,
          vcov_phi_vec = vcov_phi_vec,
          delta_t = delta_t
        )
        testthat::expect_true(
          all(
            (
              answer - summary(delta)$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::skip_on_cran()
        phi <- matrix(
          data = c(
            -0.357, 0.771, -0.450,
            0.0, -0.511, 0.729,
            0, 0, -0.693
          ),
          nrow = 3
        )
        colnames(phi) <- rownames(phi) <- c("x", "m", "y")
        vcov_phi_vec <- matrix(
          data = c(
            0.00843, 0.00040, -0.00151,
            -0.00600, -0.00033, 0.00110,
            0.00324, 0.00020, -0.00061,
            0.00040, 0.00374, 0.00016,
            -0.00022, -0.00273, -0.00016,
            0.00009, 0.00150, 0.00012,
            -0.00151, 0.00016, 0.00389,
            0.00103, -0.00007, -0.00283,
            -0.00050, 0.00000, 0.00156,
            -0.00600, -0.00022, 0.00103,
            0.00644, 0.00031, -0.00119,
            -0.00374, -0.00021, 0.00070,
            -0.00033, -0.00273, -0.00007,
            0.00031, 0.00287, 0.00013,
            -0.00014, -0.00170, -0.00012,
            0.00110, -0.00016, -0.00283,
            -0.00119, 0.00013, 0.00297,
            0.00063, -0.00004, -0.00177,
            0.00324, 0.00009, -0.00050,
            -0.00374, -0.00014, 0.00063,
            0.00495, 0.00024, -0.00093,
            0.00020, 0.00150, 0.00000,
            -0.00021, -0.00170, -0.00004,
            0.00024, 0.00214, 0.00012,
            -0.00061, 0.00012, 0.00156,
            0.00070, -0.00012, -0.00177,
            -0.00093, 0.00012, 0.00223
          ),
          nrow = 9
        )
        delta <- DeltaDirectCentral(
          phi = phi,
          vcov_phi_vec = vcov_phi_vec,
          delta_t = 1:5
        )
        print(delta)
        summary(delta)
        confint(delta, level = 0.95)
        plot(delta)
        delta <- DeltaDirectCentral(
          phi = phi,
          vcov_phi_vec = vcov_phi_vec,
          delta_t = 1
        )
        print(delta)
        summary(delta)
        confint(delta, level = 0.95)
        testthat::expect_error(
          plot(delta)
        )
      }
    )
  },
  text = "test-cTMed-delta-direct-central",
  tol = 0.01
)

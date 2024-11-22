## ---- test-delta-med-std
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    total <- 0.07576192
    direct <- -0.3042806
    indirect <- 0.3800425
    answer <- c(
      total,
      direct,
      indirect
    )
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
    sigma <- matrix(
      data = c(
        0.24, 0.02, -0.05,
        0.02, 0.07, 0.02,
        -0.05, 0.02, 0.08
      ),
      nrow = 3
    )
    vcov_sigma_vech <- matrix(
      data = c(
        0.00057, 0.00001, -0.00009,
        0.00000, 0.00000, 0.00001,
        0.00001, 0.00012, 0.00001,
        0.00000, -0.00002, 0.00000,
        -0.00009, 0.00001, 0.00014,
        0.00000, 0.00000, -0.00005,
        0.00000, 0.00000, 0.00000,
        0.00010, 0.00001, 0.00000,
        0.00000, -0.00002, 0.00000,
        0.00001, 0.00005, 0.00001,
        0.00001, 0.00000, -0.00005,
        0.00000, 0.00001, 0.00012
      ),
      nrow = 6
    )
    delta <- DeltaMedStd(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      sigma = sigma,
      vcov_sigma_vech = vcov_sigma_vech,
      delta_t = 2,
      from = "x",
      to = "y",
      med = "m"
    )
    testthat::test_that(
      paste(text, "DeltaMedStd"),
      {
        testthat::expect_true(
          all(
            (
              answer - summary(delta)$est
            ) <= tol
          )
        )
      }
    )
    delta <- DeltaMedStd(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      sigma = sigma,
      vcov_sigma_vech = vcov_sigma_vech,
      delta_t = 1:5,
      from = "x",
      to = "y",
      med = "m"
    )
    print(delta)
    summary(delta)
    confint(delta, level = 0.95)
    plot(delta)
    delta <- DeltaMedStd(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      sigma = sigma,
      vcov_sigma_vech = vcov_sigma_vech,
      delta_t = 1,
      from = "x",
      to = "y",
      med = "m"
    )
    print(delta)
    summary(delta)
    confint(delta, level = 0.95)
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::expect_error(
          plot(delta)
        )
      }
    )
  },
  text = "test-delta-med-std",
  tol = 0.00001
)

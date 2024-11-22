## ---- test-mc-indirect-central
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    answer <- c(
      0.0000000,
      0.4008043,
      0.0000000
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
    mc <- MCIndirectCentral(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      delta_t = 2,
      R = 1000,
      seed = 42
    )
    testthat::test_that(
      paste(text, "MCIndirectCentral"),
      {
        testthat::expect_true(
          all(
            (
              answer - summary(mc)$est
            ) <= tol
          )
        )
      }
    )
    mc <- MCIndirectCentral(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      delta_t = 1:5,
      R = 1000,
      seed = NULL,
      test_phi = FALSE
    )
    print(mc)
    summary(mc)
    confint(mc, level = 0.95)
    plot(mc)
    mc <- MCIndirectCentral(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      delta_t = 1,
      R = 1000,
      seed = NULL,
      test_phi = FALSE
    )
    print(mc)
    summary(mc)
    confint(mc, level = 0.95)
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::expect_error(
          plot(mc)
        )
      }
    )
  },
  text = "test-mc-indirect-central",
  tol = 0.00001
)

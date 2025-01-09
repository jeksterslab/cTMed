## ---- test-posterior-beta
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
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
    answer <- as.vector(
      expm::expm(delta_t * phi)
    )
    phi <- MCPhi(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      R = 1000L,
      seed = 42
    )$output
    posterior <- PosteriorBeta(
      phi = phi,
      delta_t = delta_t
    )
    testthat::test_that(
      paste(text, "PosteriorBeta"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            (
              answer - summary(posterior)$est
            ) <= tol
          )
        )
      }
    )
    posterior <- PosteriorBeta(
      phi = phi,
      delta_t = 1:5
    )
    print(posterior)
    summary(posterior)
    confint(posterior, level = 0.95)
    plot(posterior)
    posterior <- PosteriorBeta(
      phi = phi,
      delta_t = 1
    )
    print(posterior)
    summary(posterior)
    confint(posterior, level = 0.95)
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::skip_on_cran()
        testthat::expect_error(
          plot(posterior)
        )
      }
    )
  },
  text = "test-posterior-beta",
  tol = 0.01
)

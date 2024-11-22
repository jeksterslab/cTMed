## ---- test-indirect-std
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    answer <- 0.3800425
    phi <- matrix(
      data = c(
        -0.357, 0.771, -0.450,
        0.0, -0.511, 0.729,
        0, 0, -0.693
      ),
      nrow = 3
    )
    colnames(phi) <- rownames(phi) <- c("x", "m", "y")
    sigma <- matrix(
      data = c(
        0.24, 0.02, -0.05,
        0.02, 0.07, 0.02,
        -0.05, 0.02, 0.08
      ),
      nrow = 3
    )
    delta_t <- 2
    testthat::test_that(
      paste(text, "IndirectStd"),
      {
        testthat::expect_true(
          (
            answer - IndirectStd(
              phi = phi,
              sigma = sigma,
              delta_t = delta_t,
              from = "x",
              to = "y",
              med = "m"
            )$output
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "MedStd"),
      {
        testthat::expect_true(
          (
            answer - MedStd(
              phi = phi,
              sigma = sigma,
              delta_t = delta_t,
              from = "x",
              to = "y",
              med = "m"
            )$output[, "indirect"]
          ) <= tol
        )
      }
    )
    med <- MedStd(
      phi = phi,
      sigma = sigma,
      delta_t = 1:5,
      from = "x",
      to = "y",
      med = "m"
    )
    print(med)
    summary(med)
    plot(med)
    plot(med, col = 1:3)
    med <- Med(
      phi = phi,
      delta_t = 1,
      from = "x",
      to = "y",
      med = "m"
    )
    print(med)
    summary(med)
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::expect_error(
          plot(med)
        )
      }
    )
    indirect <- IndirectStd(
      phi = phi,
      sigma = sigma,
      delta_t = delta_t,
      from = "x",
      to = "y",
      med = "m"
    )
    print(indirect)
  },
  text = "test-indirect-std",
  tol = 0.00001
)

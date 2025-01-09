## ---- test-indirect
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    answer <- 0.4008043
    phi <- matrix(
      data = c(
        -0.357, 0.771, -0.450,
        0.0, -0.511, 0.729,
        0, 0, -0.693
      ),
      nrow = 3
    )
    colnames(phi) <- rownames(phi) <- c("x", "m", "y")
    delta_t <- 2
    testthat::test_that(
      paste(text, "Indirect"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          (
            answer - Indirect(
              phi = phi,
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
      paste(text, "Med"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          (
            answer - Med(
              phi = phi,
              delta_t = delta_t,
              from = "x",
              to = "y",
              med = "m"
            )$output[, "indirect"]
          ) <= tol
        )
      }
    )
    med <- Med(
      phi = phi,
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
        testthat::skip_on_cran()
        testthat::expect_error(
          plot(med)
        )
      }
    )
    indirect <- Indirect(
      phi = phi,
      delta_t = delta_t,
      from = "x",
      to = "y",
      med = "m"
    )
    print(indirect)
  },
  text = "test-indirect",
  tol = 0.01
)

## ---- test-test-stable
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    answer <- 0.0799008
    phi <- matrix(
      data = c(
        -0.357, 0.771, -0.450,
        0.0, -0.511, 0.729,
        0, 0, -0.693
      ),
      nrow = 3
    )
    testthat::test_that(
      paste(text, "TestStable"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          cTMed:::.TestStable(phi)
        )
      }
    )
  },
  text = "test-test-stable",
  tol = 0.01
)

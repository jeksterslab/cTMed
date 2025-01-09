## ---- test-wald-probs-of-alpha-dot
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    testthat::test_that(
      text,
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            abs(
              c(2.5, 97.5) - cTMed:::.ProbsofAlpha(alpha = 0.05) * 100
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-wald-probs-of-alpha-dot",
  tol = 0.01
)

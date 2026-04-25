## ---- test-cTMed-wald-probs-of-alpha-dot
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    set.seed(42)
    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }
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
  text = "test-cTMed-wald-probs-of-alpha-dot",
  tol = 0.01
)

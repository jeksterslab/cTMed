## ---- test-cTMed-stable
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
      paste(text, "TestStable"),
      {
        testthat::skip_on_cran()
        answer <- 0.0799008
        phi <- matrix(
          data = c(
            -0.357, 0.771, -0.450,
            0.0, -0.511, 0.729,
            0, 0, -0.693
          ),
          nrow = 3
        )
        testthat::expect_true(
          cTMed:::.TestStable(phi)
        )
      }
    )
  },
  text = "test-cTMed-stable",
  tol = 0.01
)

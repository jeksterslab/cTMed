## ---- test
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    testthat::test_that(
      "true",
      {
        testthat::expect_true(
          TRUE
        )
      }
    )
  },
  text = "test"
)

## ---- test-wald-ci-wald-ci-dot
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 n,
                 tol) {
    message(text)
    x <- data.frame(
      x1 = rnorm(n = n),
      x2 = rnorm(n = n),
      x3 = rnorm(n = n)
    )
    x$y <- data.matrix(x) %*% c(.5, .25, 0) + rnorm(n = n)
    lm_obj <- stats::lm(
      formula = y ~ .,
      data = x
    )
    lm_ci <- confint(lm_obj)
    k <- ncol(x)
    student <- cTMed:::.CIWald(
      est = coef(lm_obj),
      se = sqrt(diag(vcov(lm_obj))),
      alpha = 0.05,
      z = FALSE,
      df = n - k,
      test = FALSE
    )
    z <- cTMed:::.CIWald(
      est = coef(lm_obj),
      se = sqrt(diag(vcov(lm_obj))),
      alpha = 0.05,
      z = TRUE,
      test = FALSE
    )
    testthat::test_that(
      paste(text, "t"),
      {
        testthat::expect_true(
          all(
            abs(
              lm_ci[, 1] - student[, 6]
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              lm_ci[, 2] - student[, 7]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "z"),
      {
        testthat::expect_true(
          all(
            abs(
              lm_ci[, 1] - z[, 5]
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              lm_ci[, 2] - z[, 6]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-wald-ci-wald-ci-dot",
  n = 10^6,
  tol = 0.0001
)

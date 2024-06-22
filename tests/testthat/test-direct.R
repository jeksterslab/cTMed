## ---- test-direct
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    answer <- -0.3209035
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
      paste(text, "Direct"),
      {
        testthat::expect_true(
          (
            answer - Direct(
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
      paste(text, "DirectVec"),
      {
        testthat::expect_true(
          (
            answer - cTMed:::.DirectVec(
              phi = as.vector(phi),
              delta_t = delta_t,
              from = 1,
              to = 3,
              med = 2
            )
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "Med"),
      {
        testthat::expect_true(
          (
            answer - Med(
              phi = phi,
              delta_t = delta_t,
              from = "x",
              to = "y",
              med = "m"
            )$output[, "direct"]
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
  },
  text = "test-direct",
  tol = 0.00001
)

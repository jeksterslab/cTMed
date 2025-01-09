## ---- test-total
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
    colnames(phi) <- rownames(phi) <- c("x", "m", "y")
    delta_t <- 2
    testthat::test_that(
      paste(text, "Total"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          (
            answer - Total(
              phi = phi,
              delta_t = delta_t
            )$output[3, 1]
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "TotalVec"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            (
              as.vector(
                expm::expm(delta_t * phi)
              ) - as.vector(
                cTMed:::.TotalVec(
                  phi_vec = as.vector(phi),
                  delta_t = delta_t
                )
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "TotalDeltaT"),
      {
        testthat::skip_on_cran()
        testthat::expect_true(
          all(
            (
              as.vector(
                expm::expm(delta_t * phi)
              ) - as.vector(
                cTMed:::.TotalDeltaT(
                  phi = phi,
                  delta_t = delta_t
                )
              )[seq_len(3 * 3)]
            ) <= tol
          )
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
            )$output[, "total"]
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
    total <- Total(
      phi = phi,
      delta_t = delta_t
    )
    print(total)
  },
  text = "test-total",
  tol = 0.01
)

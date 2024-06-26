## ---- test-total-central
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    delta_t <- 2
    answer <- c(
      0.7297791,
      0.4398068,
      0.0000000,
      delta_t
    )
    phi <- matrix(
      data = c(
        -0.357, 0.771, -0.450,
        0.0, -0.511, 0.729,
        0, 0, -0.693
      ),
      nrow = 3
    )
    colnames(phi) <- rownames(phi) <- c("x", "m", "y")
    testthat::test_that(
      paste(text, "TotalCentral"),
      {
        testthat::expect_true(
          all(
            (
              answer - TotalCentral(
                phi = phi,
                delta_t = delta_t
              )$output
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "TotalCentrals"),
      {
        testthat::expect_true(
          all(
            (
              answer - c(
                as.vector(
                  cTMed:::.TotalCentrals(
                    phi = phi,
                    delta_t = delta_t
                  )
                ),
                delta_t
              )
            ) <= tol
          )
        )
      }
    )
    total_central <- TotalCentral(
      phi = phi,
      delta_t = 1:5
    )
    print(total_central)
    summary(total_central)
    plot(total_central)
    total_central <- TotalCentral(
      phi = phi,
      delta_t = 1
    )
    print(total_central)
    summary(total_central)
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::expect_error(
          plot(total_central)
        )
      }
    )
  },
  text = "test-total-central",
  tol = 0.00001
)

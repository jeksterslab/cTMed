## ---- test-indirect-central
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    delta_t <- 2
    answer <- c(
      0.0000000,
      0.4008043,
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
      paste(text, "IndirectCentral"),
      {
        testthat::expect_true(
          all(
            (
              answer - IndirectCentral(
                phi = phi,
                delta_t = delta_t
              )$output
            ) <= tol
          )
        )
      }
    )
    # testthat::test_that(
    #   paste(text, "IndirectCentrals"),
    #   {
    #     testthat::expect_true(
    #       all(
    #         (
    #           answer - c(
    #             as.vector(
    #               cTMed:::.IndirectCentrals(
    #                 phi = phi,
    #                 delta_t = delta_t
    #               )
    #             ),
    #             delta_t
    #           )
    #         ) <= tol
    #       )
    #     )
    #   }
    # )
    indirect_central <- IndirectCentral(
      phi = phi,
      delta_t = 1:5
    )
    print(indirect_central)
    summary(indirect_central)
    plot(indirect_central)
    indirect_central <- IndirectCentral(
      phi = phi,
      delta_t = 1
    )
    print(indirect_central)
    summary(indirect_central)
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::expect_error(
          plot(indirect_central)
        )
      }
    )
  },
  text = "test-indirect-central",
  tol = 0.01
)

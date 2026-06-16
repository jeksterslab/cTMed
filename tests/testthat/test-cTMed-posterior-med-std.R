## ---- test-cTMed-posterior-med-std
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }
    testthat::test_that(
      paste(text, "PosteriorMedStd"),
      {
        testthat::skip_on_cran()
        phi <- matrix(
          data = c(
            -0.357, 0.771, -0.450,
            0.0, -0.511, 0.729,
            0, 0, -0.693
          ),
          nrow = 3
        )
        colnames(phi) <- rownames(phi) <- c("x", "m", "y")
        sigma <- matrix(
          data = c(
            0.24455556, 0.02201587, -0.05004762,
            0.02201587, 0.07067800, 0.01539456,
            -0.05004762, 0.01539456, 0.07553061
          ),
          nrow = 3
        )
        colnames(sigma) <- rownames(sigma) <- c("x", "m", "y")
        delta_t <- 2
        answer <- MedStd(
          phi = phi,
          sigma = sigma,
          delta_t = delta_t,
          from = "x",
          to = "y",
          med = "m"
        )$output[1, 1:3]
        posterior <- PosteriorMedStd(
          phi = rep(
            x = list(phi),
            times = 10
          ),
          sigma = rep(
            x = list(sigma),
            times = 10
          ),
          delta_t = delta_t,
          from = "x",
          to = "y",
          med = "m"
        )
        testthat::expect_true(
          all(
            abs(
              answer - summary(posterior)$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::skip_on_cran()
        phi <- matrix(
          data = c(
            -0.357, 0.771, -0.450,
            0.0, -0.511, 0.729,
            0, 0, -0.693
          ),
          nrow = 3
        )
        colnames(phi) <- rownames(phi) <- c("x", "m", "y")
        sigma <- matrix(
          data = c(
            0.24455556, 0.02201587, -0.05004762,
            0.02201587, 0.07067800, 0.01539456,
            -0.05004762, 0.01539456, 0.07553061
          ),
          nrow = 3
        )
        colnames(sigma) <- rownames(sigma) <- c("x", "m", "y")
        posterior <- PosteriorMedStd(
          phi = rep(
            x = list(phi),
            times = 10
          ),
          sigma = rep(
            x = list(sigma),
            times = 10
          ),
          delta_t = 1:5,
          from = "x",
          to = "y",
          med = "m"
        )
        print(posterior)
        summary(posterior)
        confint(posterior, level = 0.95)
        plot(posterior)
        posterior <- PosteriorMedStd(
          phi = rep(
            x = list(phi),
            times = 10
          ),
          sigma = rep(
            x = list(sigma),
            times = 10
          ),
          delta_t = 1,
          from = "x",
          to = "y",
          med = "m"
        )
        print(posterior)
        summary(posterior)
        confint(posterior, level = 0.95)
        testthat::expect_error(
          plot(posterior)
        )
      }
    )
  },
  text = "test-cTMed-posterior-med-std",
  tol = 1e-06
)

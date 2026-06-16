## ---- test-cTMed-posterior-indirect-central-std
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
      paste(text, "PosteriorIndirectCentralStd"),
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

        phi_post <- lapply(
          X = seq_len(10),
          FUN = function(j) {
            out <- phi
            out[1, 1] <- out[1, 1] - (j - 5) * 0.0001
            out[2, 2] <- out[2, 2] - (j - 5) * 0.0001
            out
          }
        )
        sigma_post <- lapply(
          X = seq_len(10),
          FUN = function(j) {
            out <- sigma
            diag(out) <- diag(out) + j * 0.0001
            out
          }
        )
        output <- PosteriorIndirectCentralStd(
          phi = phi_post,
          sigma = sigma_post,
          delta_t = 1:2
        )
        testthat::expect_s3_class(output, "ctmedmc")
        testthat::expect_true(output$args$standardized)
        testthat::expect_equal(
          output$output[[1]]$est,
          colMeans(output$output[[1]]$thetahatstar),
          tolerance = tol
        )
        testthat::expect_equal(
          dim(output$output[[1]]$thetahatstar),
          c(10L, 4L)
        )
        testthat::expect_equal(
          colnames(output$output[[1]]$thetahatstar),
          c("x", "m", "y", "interval")
        )
        print(output)
        summary(output)
        confint(output)
        plot(output)
      }
    )
  },
  text = "test-cTMed-posterior-indirect-central-std",
  tol = 0.01
)

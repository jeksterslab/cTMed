## ---- test-cTMed-delta-direct-central-std
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
      paste(text, "DeltaDirectCentralStd"),
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

        q <- length(cTMed:::.Vec(phi)) + length(cTMed:::.Vech(sigma))
        vcov_theta <- diag(1.0e-6, q)
        delta_t <- 1:2
        output <- DeltaDirectCentralStd(
          phi = phi,
          sigma = sigma,
          vcov_theta = vcov_theta,
          delta_t = delta_t
        )
        testthat::expect_s3_class(output, "ctmeddelta")
        testthat::expect_true(output$args$standardized)
        testthat::expect_equal(
          as.numeric(output$output[[1]]$est),
          as.numeric(DirectCentralStd(
            phi = phi,
            sigma = sigma,
            delta_t = 1
          )$output[1, c("x", "m", "y")]),
          tolerance = tol
        )
        testthat::expect_equal(
          dim(output$output[[1]]$jacobian),
          c(3L, q)
        )
        testthat::expect_equal(
          dim(output$output[[1]]$vcov),
          c(3L, 3L)
        )
        print(output)
        summary(output)
        confint(output)
        plot(output)
      }
    )
    testthat::test_that(
      paste(text, "DeltaDirectCentralStd diagonal sigma"),
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

        sigma <- diag(diag(sigma))
        colnames(sigma) <- rownames(sigma) <- c("x", "m", "y")
        q <- length(cTMed:::.Vec(phi)) + length(diag(sigma))
        vcov_theta <- diag(1.0e-6, q)
        output <- DeltaDirectCentralStd(
          phi = phi,
          sigma = sigma,
          vcov_theta = vcov_theta,
          delta_t = 1,
          sigma_diag = TRUE
        )
        testthat::expect_s3_class(output, "ctmeddelta")
        testthat::expect_equal(
          as.numeric(output$output[[1]]$est),
          as.numeric(DirectCentralStd(
            phi = phi,
            sigma = sigma,
            delta_t = 1
          )$output[1, c("x", "m", "y")]),
          tolerance = tol
        )
        testthat::expect_equal(
          dim(output$output[[1]]$jacobian),
          c(3L, q)
        )
      }
    )
  },
  text = "test-cTMed-delta-direct-central-std",
  tol = 0.01
)

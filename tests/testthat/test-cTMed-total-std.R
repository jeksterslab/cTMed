## ---- test-cTMed-total-std
# nolint start: cyclocomp_linter
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "TotalStd"),
      {
        testthat::skip_on_cran()
        answer <- 0.07477656
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
        delta_t <- 2
        total <- expm::expm(delta_t * phi)
        total_cov <- ExpCov(
          phi = phi,
          sigma = sigma,
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(0, 0, 0),
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(.5, .3, .4),
          delta_t = delta_t
        )
        total_std <- matrix(
          data = 0.0,
          nrow = 3,
          ncol = 3
        )
        for (j in 1:3) {
          for (i in 1:3) {
            total_std[i, j] <- (
              sqrt(total_cov[i, i]) * total[i, j]
            ) * (1 / sqrt(total_cov[j, j]))
          }
        }
        testthat::expect_true(
          (
            answer - TotalStd(
              phi = phi,
              sigma = sigma,
              delta_t = delta_t
            )$output[3, 1]
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "TotalStdVec"),
      {
        testthat::skip_on_cran()
        answer <- 0.07477656
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
        delta_t <- 2
        total <- expm::expm(delta_t * phi)
        total_cov <- ExpCov(
          phi = phi,
          sigma = sigma,
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(0, 0, 0),
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(.5, .3, .4),
          delta_t = delta_t
        )
        total_std <- matrix(
          data = 0.0,
          nrow = 3,
          ncol = 3
        )
        for (j in 1:3) {
          for (i in 1:3) {
            total_std[i, j] <- (
              sqrt(total_cov[i, i]) * total[i, j]
            ) * (1 / sqrt(total_cov[j, j]))
          }
        }
        testthat::expect_true(
          all(
            (
              as.vector(
                total_std
              ) - as.vector(
                cTMed:::.TotalStdVec(
                  v = c(cTMed:::.Vec(phi), cTMed:::.Vech(sigma)),
                  delta_t = delta_t
                )
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "TotalStdDeltaT"),
      {
        testthat::skip_on_cran()
        answer <- 0.07477656
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
        delta_t <- 2
        total <- expm::expm(delta_t * phi)
        total_cov <- ExpCov(
          phi = phi,
          sigma = sigma,
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(0, 0, 0),
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(.5, .3, .4),
          delta_t = delta_t
        )
        total_std <- matrix(
          data = 0.0,
          nrow = 3,
          ncol = 3
        )
        for (j in 1:3) {
          for (i in 1:3) {
            total_std[i, j] <- (
              sqrt(total_cov[i, i]) * total[i, j]
            ) * (1 / sqrt(total_cov[j, j]))
          }
        }
        testthat::expect_true(
          all(
            (
              as.vector(
                total_std
              ) - as.vector(
                cTMed:::.TotalStdDeltaT(
                  phi = phi,
                  sigma = sigma,
                  delta_t = delta_t
                )
              )[seq_len(3 * 3)]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "MedStd"),
      {
        testthat::skip_on_cran()
        answer <- 0.07477656
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
        delta_t <- 2
        total <- expm::expm(delta_t * phi)
        total_cov <- ExpCov(
          phi = phi,
          sigma = sigma,
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(0, 0, 0),
          delta_t = delta_t
        )
        ExpMean(
          phi = phi,
          iota = c(.5, .3, .4),
          delta_t = delta_t
        )
        total_std <- matrix(
          data = 0.0,
          nrow = 3,
          ncol = 3
        )
        for (j in 1:3) {
          for (i in 1:3) {
            total_std[i, j] <- (
              sqrt(total_cov[i, i]) * total[i, j]
            ) * (1 / sqrt(total_cov[j, j]))
          }
        }
        testthat::expect_true(
          (
            answer - MedStd(
              phi = phi,
              sigma = sigma,
              delta_t = delta_t,
              from = "x",
              to = "y",
              med = "m"
            )$output[, "total"]
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::skip_on_cran()
        answer <- 0.07477656
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
        med <- MedStd(
          phi = phi,
          sigma = sigma,
          delta_t = 1:5,
          from = "x",
          to = "y",
          med = "m"
        )
        print(med)
        summary(med)
        plot(med)
        plot(med, col = 1:3)
        med <- MedStd(
          phi = phi,
          sigma = sigma,
          delta_t = 1,
          from = "x",
          to = "y",
          med = "m"
        )
        print(med)
        summary(med)
        testthat::expect_error(
          plot(med)
        )
        total <- TotalStd(
          phi = phi,
          sigma = sigma,
          delta_t = 2
        )
        print(total)
      }
    )
  },
  text = "test-cTMed-total-std",
  tol = 0.01
)
# nolint end

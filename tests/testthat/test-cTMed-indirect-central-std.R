## ---- test-cTMed-indirect-central-std

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
    if (requireNamespace("expm") && requireNamespace("simStateSpace")) {
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

      testthat::test_that(
        paste(text, "IndirectCentralStd"),
        {
          testthat::skip_on_cran()
          p <- nrow(phi)
          delta_t <- 2
          total <- expm::expm(delta_t * phi)
          cov_eta <- simStateSpace::LinSDECovEta(
            phi = phi,
            sigma = sigma
          )
          total_std <- total
          for (j in seq_len(ncol(total))) {
            for (i in seq_len(nrow(total))) {
              total_std[i, j] <- sqrt(cov_eta[j, j]) * total[i, j] /
                sqrt(cov_eta[i, i])
            }
          }
          expected <- rep(
            x = 0.0,
            times = p
          )
          for (m in seq_len(p)) {
            d <- diag(p)
            d[m, m] <- 0
            direct <- expm::expm(
              delta_t * d %*% phi %*% d
            )
            direct_std <- direct
            for (j in seq_len(ncol(direct))) {
              for (i in seq_len(nrow(direct))) {
                direct_std[i, j] <- sqrt(cov_eta[j, j]) * direct[i, j] /
                  sqrt(cov_eta[i, i])
              }
            }
            for (j in seq_len(p)) {
              for (i in seq_len(p)) {
                if (!(m == i || m == j || i == j)) {
                  expected[m] <- expected[m] + total_std[i, j] -
                    direct_std[i, j]
                }
              }
            }
          }
          expected <- c(
            expected,
            delta_t
          )
          obtained <- IndirectCentralStd(
            phi = phi,
            sigma = sigma,
            delta_t = delta_t
          )$output
          testthat::expect_true(
            all(abs(expected - obtained) <= tol)
          )
        }
      )
      testthat::test_that(
        paste(text, "IndirectCentralStds"),
        {
          testthat::skip_on_cran()
          p <- nrow(phi)
          delta_t <- 2
          total <- expm::expm(delta_t * phi)
          cov_eta <- simStateSpace::LinSDECovEta(
            phi = phi,
            sigma = sigma
          )
          total_std <- total
          for (j in seq_len(ncol(total))) {
            for (i in seq_len(nrow(total))) {
              total_std[i, j] <- sqrt(cov_eta[j, j]) * total[i, j] /
                sqrt(cov_eta[i, i])
            }
          }
          expected <- rep(
            x = 0.0,
            times = p
          )
          for (m in seq_len(p)) {
            d <- diag(p)
            d[m, m] <- 0
            direct <- expm::expm(
              delta_t * d %*% phi %*% d
            )
            direct_std <- direct
            for (j in seq_len(ncol(direct))) {
              for (i in seq_len(nrow(direct))) {
                direct_std[i, j] <- sqrt(cov_eta[j, j]) * direct[i, j] /
                  sqrt(cov_eta[i, i])
              }
            }
            for (j in seq_len(p)) {
              for (i in seq_len(p)) {
                if (!(m == i || m == j || i == j)) {
                  expected[m] <- expected[m] + total_std[i, j] -
                    direct_std[i, j]
                }
              }
            }
          }
          expected <- c(
            expected,
            delta_t
          )
          obtained <- c(
            as.vector(
              cTMed:::.IndirectCentralStds(
                phi = phi,
                sigma = sigma,
                delta_t = delta_t
              )
            ),
            delta_t
          )
          testthat::expect_true(
            all(abs(expected - obtained) <= tol)
          )
        }
      )
      testthat::test_that(
        paste(text, "plot error"),
        {
          testthat::skip_on_cran()
          indirect_central <- IndirectCentralStd(
            phi = phi,
            sigma = sigma,
            delta_t = 1:5
          )
          print(indirect_central)
          summary(indirect_central)
          plot(indirect_central)
          indirect_central <- IndirectCentralStd(
            phi = phi,
            sigma = sigma,
            delta_t = 1
          )
          print(indirect_central)
          summary(indirect_central)
          testthat::expect_error(
            plot(indirect_central)
          )
        }
      )
    }
  },
  text = "test-cTMed-indirect-central-std",
  tol = 0.01
)

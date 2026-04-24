## ---- test-cTMed-direct-central
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "DirectCentral"),
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
        delta_t <- 2
        answer <- rep(
          x = 0.0,
          times = nrow(phi)
        )
        for (m in seq_len(nrow(phi))) {
          d <- diag(nrow(phi))
          d[m, m] <- 0
          direct <- expm::expm(
            delta_t * d %*% phi %*% d
          )
          for (j in seq_len(nrow(phi))) {
            for (i in seq_len(nrow(phi))) {
              if (!(m == i || m == j || i == j)) {
                answer[m] <- answer[m] + direct[i, j]
              }
            }
          }
        }
        testthat::expect_true(
          all(
            (
              answer - DirectCentral(
                phi = phi,
                delta_t = delta_t
              )$output
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "DirectCentrals"),
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
        delta_t <- 2
        answer <- rep(
          x = 0.0,
          times = nrow(phi)
        )
        for (m in seq_len(nrow(phi))) {
          d <- diag(nrow(phi))
          d[m, m] <- 0
          direct <- expm::expm(
            delta_t * d %*% phi %*% d
          )
          for (j in seq_len(nrow(phi))) {
            for (i in seq_len(nrow(phi))) {
              if (!(m == i || m == j || i == j)) {
                answer[m] <- answer[m] + direct[i, j]
              }
            }
          }
        }
        testthat::expect_true(
          all(
            (
              answer - c(
                as.vector(
                  cTMed:::.DirectCentrals(
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
        direct_central <- DirectCentral(
          phi = phi,
          delta_t = 1:5
        )
        print(direct_central)
        summary(direct_central)
        plot(direct_central)
        direct_central <- DirectCentral(
          phi = phi,
          delta_t = 1
        )
        print(direct_central)
        summary(direct_central)
        testthat::expect_error(
          plot(direct_central)
        )
      }
    )
  },
  text = "test-cTMed-direct-central",
  tol = 0.01
)

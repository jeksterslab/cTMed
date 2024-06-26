## ---- test-mc-med
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    total <- 0.0799008
    direct <- -0.3209035
    indirect <- 0.4008043
    answer <- c(
      total,
      direct,
      indirect
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
    vcov_phi_vec <- matrix(
      data = c(
        0.002704274, -0.001475275, 0.000949122,
        -0.001619422, 0.000885122, -0.000569404,
        0.00085493, -0.000465824, 0.000297815,
        -0.001475275, 0.004428442, -0.002642303,
        0.000980573, -0.00271817, 0.001618805,
        -0.000586921, 0.001478421, -0.000871547,
        0.000949122, -0.002642303, 0.006402668,
        -0.000697798, 0.001813471, -0.004043138,
        0.000463086, -0.001120949, 0.002271711,
        -0.001619422, 0.000980573, -0.000697798,
        0.002079286, -0.001152501, 0.000753,
        -0.001528701, 0.000820587, -0.000517524,
        0.000885122, -0.00271817, 0.001813471,
        -0.001152501, 0.00342605, -0.002075005,
        0.000899165, -0.002532849, 0.001475579,
        -0.000569404, 0.001618805, -0.004043138,
        0.000753, -0.002075005, 0.004984032,
        -0.000622255, 0.001634917, -0.003705661,
        0.00085493, -0.000586921, 0.000463086,
        -0.001528701, 0.000899165, -0.000622255,
        0.002060076, -0.001096684, 0.000686386,
        -0.000465824, 0.001478421, -0.001120949,
        0.000820587, -0.002532849, 0.001634917,
        -0.001096684, 0.003328692, -0.001926088,
        0.000297815, -0.000871547, 0.002271711,
        -0.000517524, 0.001475579, -0.003705661,
        0.000686386, -0.001926088, 0.004726235
      ),
      nrow = 9
    )
    mc <- MCMed(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      delta_t = 2,
      from = "x",
      to = "y",
      med = "m",
      R = 1000,
      seed = 42
    )
    testthat::test_that(
      paste(text, "MCMed"),
      {
        testthat::expect_true(
          all(
            (
              answer - summary(mc)$est
            ) <= tol
          )
        )
      }
    )
    mc <- MCMed(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      delta_t = 1:5,
      from = "x",
      to = "y",
      med = "m",
      R = 1000,
      seed = NULL,
      test_phi = FALSE
    )
    print(mc)
    summary(mc)
    confint(mc, level = 0.95)
    plot(mc)
    mc <- MCMed(
      phi = phi,
      vcov_phi_vec = vcov_phi_vec,
      delta_t = 1,
      from = "x",
      to = "y",
      med = "m",
      R = 1000,
      seed = NULL,
      test_phi = FALSE
    )
    print(mc)
    summary(mc)
    confint(mc, level = 0.95)
    testthat::test_that(
      paste(text, "plot error"),
      {
        testthat::expect_error(
          plot(mc)
        )
      }
    )
  },
  text = "test-mc-med",
  tol = 0.00001
)

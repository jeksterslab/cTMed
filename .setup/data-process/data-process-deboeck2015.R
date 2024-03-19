data_process_deboeck2015 <- function(overwrite = FALSE) {
  # find root directory
  root <- rprojroot::is_rstudio_project
  deboeck2015_ssm_rds <- root$find_file(
    ".setup",
    "data-raw",
    "deboeck2015_ssm.Rds"
  )
  deboeck2015_rds <- root$find_file(
    ".setup",
    "data-raw",
    "deboeck2015.Rds"
  )
  deboeck2015_na_rds <- root$find_file(
    ".setup",
    "data-raw",
    "deboeck2015_na.Rds"
  )
  deboeck2015_rda <- root$find_file(
    "data",
    "deboeck2015.rda"
  )
  if (file.exists(deboeck2015_ssm_rds)) {
    run <- FALSE
    if (overwrite) {
      run <- TRUE
    }
  } else {
    run <- TRUE
  }
  if (run) {
    dir.create(
      root$find_file(
        ".setup",
        "data-raw"
      ),
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(
      root$find_file(
        "data"
      ),
      showWarnings = FALSE,
      recursive = TRUE
    )
    set.seed(42)
    n <- 50
    time <- 7
    k <- p <- 3
    iden <- diag(k)
    null_vec <- rep(x = 0, times = k)
    mu0 <- null_vec
    sigma0 <- matrix(
      data = c(
        1.0,
        0.2,
        0.2,
        0.2,
        1.0,
        0.2,
        0.2,
        0.2,
        1.0
      ),
      nrow = 3
    )
    sigma0_l <- t(chol(sigma0))
    alpha <- null_vec
    beta <- matrix(
      data = c(
        0.7,
        0.5,
        -0.1,
        0.0,
        0.6,
        0.4,
        0,
        0,
        0.5
      ),
      nrow = k
    )
    psi <- 0.1 * iden
    psi_l <- t(chol(psi))
    library(simStateSpace)
    ssm <- SimSSMVARFixed(
      n = n,
      time = time,
      mu0 = mu0,
      sigma0_l = sigma0_l,
      alpha = alpha,
      beta = beta,
      psi_l = psi_l
    )
    data <- as.data.frame(ssm)
    colnames(data) <- c(
      "id",
      "time",
      "x",
      "m",
      "y"
    )
    deboeck2015 <- data
    deboeck2015_na <- dynUtils::InsertNA(
      data = deboeck2015,
      id = "id",
      time = "time",
      observed = c("x", "m", "y"),
      delta_t = 0.10,
      ncores = parallel::detectCores()
    )
    saveRDS(
      ssm,
      file = deboeck2015_ssm_rds,
      compress = "xz"
    )
    saveRDS(
      data,
      file = deboeck2015_rds,
      compress = "xz"
    )
    saveRDS(
      deboeck2015_na,
      file = deboeck2015_na_rds,
      compress = "xz"
    )
    save(
      deboeck2015,
      file = deboeck2015_rda,
      compress = "xz"
    )
  }
}
data_process_deboeck2015()
rm(data_process_deboeck2015)

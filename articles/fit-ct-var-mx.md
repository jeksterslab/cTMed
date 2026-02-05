# Fit the Continuous-Time Vector Autoregressive Model Using the OpenMx Package

## Data Generation Using the `SimSSMOUFixed` Function from the `simStateSpace` Package

``` r

n
#> [1] 100
time
#> [1] 1000
delta_t
#> [1] 0.1
mu0
#> [1] 0 0 0
sigma0
#>      [,1] [,2] [,3]
#> [1,]  1.0  0.2  0.2
#> [2,]  0.2  1.0  0.2
#> [3,]  0.2  0.2  1.0
sigma0_l # sigma0_l <- t(chol(sigma0))
#>      [,1]      [,2]      [,3]
#> [1,]  1.0 0.0000000 0.0000000
#> [2,]  0.2 0.9797959 0.0000000
#> [3,]  0.2 0.1632993 0.9660918
mu
#> [1] 0 0 0
phi
#>        [,1]   [,2]   [,3]
#> [1,] -0.357  0.000  0.000
#> [2,]  0.771 -0.511  0.000
#> [3,] -0.450  0.729 -0.693
sigma
#>             [,1]       [,2]        [,3]
#> [1,]  0.24455556 0.02201587 -0.05004762
#> [2,]  0.02201587 0.07067800  0.01539456
#> [3,] -0.05004762 0.01539456  0.07553061
sigma_l # sigma_l <- t(chol(sigma))
#>             [,1]      [,2]     [,3]
#> [1,]  0.49452559 0.0000000 0.000000
#> [2,]  0.04451917 0.2620993 0.000000
#> [3,] -0.10120330 0.0759256 0.243975
nu
#> [1] 0 0 0
lambda
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
theta
#>      [,1] [,2] [,3]
#> [1,]  0.2  0.0  0.0
#> [2,]  0.0  0.2  0.0
#> [3,]  0.0  0.0  0.2
theta_l # theta_l <- t(chol(theta))
#>           [,1]      [,2]      [,3]
#> [1,] 0.4472136 0.0000000 0.0000000
#> [2,] 0.0000000 0.4472136 0.0000000
#> [3,] 0.0000000 0.0000000 0.4472136
```

``` r

library(simStateSpace)
sim <- SimSSMOUFixed(
  n = n,
  time = time,
  delta_t = delta_t,
  mu0 = mu0,
  sigma0_l = sigma0_l,
  mu = mu,
  phi = phi,
  sigma_l = sigma_l,
  nu = nu,
  lambda = lambda,
  theta_l = theta_l,
  type = 0
)
data <- as.data.frame(sim)
colnames(data) <- c("id", "time", "x", "m", "y")
head(data)
#>   id time          x          m        y
#> 1  1  0.0 -0.3504435 0.41877429 2.611996
#> 2  1  0.1 -0.5920330 1.07433208 1.669272
#> 3  1  0.2 -0.7619855 1.21483834 2.369837
#> 4  1  0.3 -1.6964652 0.21209722 2.128531
#> 5  1  0.4 -1.2282686 0.09950326 1.891140
#> 6  1  0.5  0.1433985 0.66784226 2.036033
```

## Model Fitting

We use the `OpenMx` package to fit the continuous-time vector
autoregressive model.

``` r

library(OpenMx)
```

### Prepare the Initial Condition

``` r

mu0 <- mxMatrix(
  type = "Full",
  nrow = 3,
  ncol = 1,
  free = TRUE,
  values = matrix(
    data = c(
      0, 0, 0
    ),
    nrow = 3,
    ncol = 1
  ),
  labels = matrix(
    data = c(
      "mu0_1_1", "mu0_2_1", "mu0_3_1"
    ),
    nrow = 3,
    ncol = 1
  ),
  lbound = NA,
  ubound = NA,
  byrow = FALSE,
  dimnames = list(
    c("eta_x", "eta_m", "eta_y"),
    "mu0"
  ),
  name = "mu0"
)
sigma0 <- mxMatrix(
  type = "Symm",
  nrow = 3,
  ncol = 3,
  free = TRUE,
  values = matrix(
    data = c(
      1.0, 0.2, 0.2,
      0.2, 1.0, 0.2,
      0.2, 0.2, 1.0
    ),
    nrow = 3,
    ncol = 3
  ),
  labels = matrix(
    data = c(
      "sigma0_1_1", "sigma0_2_1", "sigma0_3_1",
      "sigma0_2_1", "sigma0_2_2", "sigma0_3_2",
      "sigma0_3_1", "sigma0_3_2", "sigma0_3_3"
    ),
    nrow = 3,
    ncol = 3
  ),
  lbound = matrix(
    data = c(
      0, NA, NA,
      NA, 0, NA,
      NA, NA, 0
    ),
    nrow = 3,
    ncol = 3
  ),
  ubound = NA,
  byrow = FALSE,
  dimnames = list(
    c("eta_x", "eta_m", "eta_y"),
    c("eta_x", "eta_m", "eta_y")
  ),
  name = "sigma0"
)
```

### Prepare the Measurement Model

``` r

lambda <- mxMatrix(
  type = "Diag",
  nrow = 3,
  ncol = 3,
  free = FALSE,
  values = 1,
  labels = NA,
  lbound = NA,
  ubound = NA,
  byrow = FALSE,
  dimnames = list(
    c("x", "m", "y"),
    c("eta_x", "eta_m", "eta_y")
  ),
  name = "lambda"
)
```

### Prepare the Dynamic Model

``` r

phi <- mxMatrix(
  type = "Full",
  nrow = 3,
  ncol = 3,
  free = TRUE,
  values = matrix(
    data = c(
      -0.2, 0.0, 0.0,
      0.0, -0.2, 0.0,
      0.0, 0.0, -0.2
    ),
    nrow = 3,
    ncol = 3
  ),
  labels = matrix(
    data = c(
      "phi_1_1", "phi_2_1", "phi_3_1",
      "phi_1_2", "phi_2_2", "phi_3_2",
      "phi_1_3", "phi_2_3", "phi_3_3"
    ),
    nrow = 3,
    ncol = 3
  ),
  lbound = -1.5,
  ubound = 1.5,
  byrow = FALSE,
  dimnames = list(
    c("eta_x", "eta_m", "eta_y"),
    c("eta_x", "eta_m", "eta_y")
  ),
  name = "phi"
)
```

### Prepare the Noise Matrices

``` r

sigma <- mxMatrix(
  type = "Symm",
  nrow = 3,
  ncol = 3,
  free = TRUE,
  values = 0.2 * diag(3),
  labels = matrix(
    data = c(
      "sigma_1_1", "sigma_2_1", "sigma_3_1",
      "sigma_2_1", "sigma_2_2", "sigma_3_2",
      "sigma_3_1", "sigma_3_2", "sigma_3_3"
    ),
    nrow = 3,
    ncol = 3
  ),
  lbound = matrix(
    data = c(
      0, NA, NA,
      NA, 0, NA,
      NA, NA, 0
    ),
    nrow = 3,
    ncol = 3
  ),
  ubound = NA,
  byrow = FALSE,
  dimnames = list(
    c("eta_x", "eta_m", "eta_y"),
    c("eta_x", "eta_m", "eta_y")
  ),
  name = "sigma"
)
theta <- mxMatrix(
  type = "Diag",
  nrow = 3,
  ncol = 3,
  free = TRUE,
  values = 0.2 * diag(3),
  labels = matrix(
    data = c(
      "theta_1_1", "fixed", "fixed",
      "fixed", "theta_2_2", "fixed",
      "fixed", "fixed", "theta_3_3"
    ),
    nrow = 3,
    ncol = 3
  ),
  lbound = matrix(
    data = c(
      0, NA, NA,
      NA, 0, NA,
      NA, NA, 0
    ),
    nrow = 3,
    ncol = 3
  ),
  ubound = NA,
  byrow = FALSE,
  dimnames = list(
    c("x", "m", "y"),
    c("x", "m", "y")
  ),
  name = "theta"
)
```

### Prepare Miscellaneous Matrices

``` r

time <- mxMatrix(
  type = "Full",
  nrow = 1,
  ncol = 1,
  free = FALSE,
  labels = "data.time",
  name = "time"
)
gamma <- mxMatrix(
  type = "Zero",
  nrow = 3,
  ncol = 1,
  name = "gamma"
)
kappa <- mxMatrix(
  type = "Zero",
  nrow = 3,
  ncol = 1,
  name = "kappa"
)
covariate <- mxMatrix(
  type = "Zero",
  nrow = 1,
  ncol = 1,
  name = "covariate"
)
```

### Prepare the Model

In this parameterization, we fit the same model to all individuals
(`id`) using a multi-group framework, assuming that the parameters
remain fixed across individuals.

``` r

model <- mxModel(
  model = "CTVAR",
  phi,
  gamma,
  lambda,
  kappa,
  sigma,
  theta,
  mu0,
  sigma0,
  covariate,
  time,
  mxExpectationStateSpaceContinuousTime(
    A = "phi",
    B = "gamma",
    C = "lambda",
    D = "kappa",
    Q = "sigma",
    R = "theta",
    x0 = "mu0",
    P0 = "sigma0",
    u = "covariate",
    t = "time",
    dimnames = c("x", "m", "y")
  ),
  mxFitFunctionML(),
  mxData(
    observed = data,
    type = "raw"
  )
)
ids <- sort(
  unique(data[, "id"])
)
model_id <- lapply(
  X = ids,
  FUN = function(i,
                 data,
                 model) {
    return(
      mxModel(
        name = paste0("CTVAR", "_", i),
        model = model,
        mxData(
          observed = data[
            which(
              data[, "id"] == i
            ), ,
            drop = FALSE
          ],
          type = "raw"
        )
      )
    )
  },
  data = data,
  model = model
)
```

### Fit the Model

``` r

fit <- mxTryHardctsem(
  model = mxModel(
    name = "CTVAR",
    model_id,
    mxFitFunctionMultigroup(
      paste0(
        "CTVAR",
        "_",
        ids
      )
    )
  ),
  extraTries = 1000
)
#> Running CTVAR with 27 parameters
#> 
#> Beginning initial fit attempt
#> Running CTVAR with 27 parameters
#> 
#>  Lowest minimum so far:  429365.485591036
#> 
#> Solution found
```

    #> 
    #>  Solution found!  Final fit=429365.49 (started at 446038.15)  (1 attempt(s): 1 valid, 0 errors)
    #>  Start values from best fit:
    #> -0.351820812571319,0.744058348126096,-0.458865690430299,0.0173253212869458,-0.488579953983734,0.726990071828104,-0.023803730310335,-0.00993023993483604,-0.688416480027775,0.237552827206534,0.0317029444801824,0.0717951385747256,-0.053613558460382,0.012338997867509,0.0757517888198224,0.198871372142446,0.199532519044795,0.201182587340908,0.00635388230183679,-0.0424239172867786,0.13014992344374,1.150441191294,0.413787071269287,1.22186739084888,0.226069493859024,0.235392674509846,0.962739024156933
    summary(fit)
    #> Summary of CTVAR 
    #>  
    #> free parameters:
    #>          name         matrix   row   col     Estimate   Std.Error A lbound
    #> 1     phi_1_1    CTVAR_1.phi eta_x eta_x -0.351820813 0.035321153     -1.5
    #> 2     phi_2_1    CTVAR_1.phi eta_m eta_x  0.744058348 0.021718167     -1.5
    #> 3     phi_3_1    CTVAR_1.phi eta_y eta_x -0.458865690 0.022742841     -1.5
    #> 4     phi_1_2    CTVAR_1.phi eta_x eta_m  0.017325321 0.030891811     -1.5
    #> 5     phi_2_2    CTVAR_1.phi eta_m eta_m -0.488579954 0.019189836     -1.5
    #> 6     phi_3_2    CTVAR_1.phi eta_y eta_m  0.726990072 0.020119692     -1.5
    #> 7     phi_1_3    CTVAR_1.phi eta_x eta_y -0.023803730 0.023614255     -1.5
    #> 8     phi_2_3    CTVAR_1.phi eta_m eta_y -0.009930240 0.014649985     -1.5
    #> 9     phi_3_3    CTVAR_1.phi eta_y eta_y -0.688416480 0.015475497     -1.5
    #> 10  sigma_1_1  CTVAR_1.sigma eta_x eta_x  0.237552827 0.006729458        0
    #> 11  sigma_2_1  CTVAR_1.sigma eta_x eta_m  0.031702944 0.002522389         
    #> 12  sigma_2_2  CTVAR_1.sigma eta_m eta_m  0.071795139 0.001922894        0
    #> 13  sigma_3_1  CTVAR_1.sigma eta_x eta_y -0.053613558 0.002638619         
    #> 14  sigma_3_2  CTVAR_1.sigma eta_m eta_y  0.012338998 0.001396694         
    #> 15  sigma_3_3  CTVAR_1.sigma eta_y eta_y  0.075751789 0.002137223        0
    #> 16  theta_1_1  CTVAR_1.theta     x     x  0.198871372 0.001163882        0
    #> 17  theta_2_2  CTVAR_1.theta     m     m  0.199532519 0.001000026        0
    #> 18  theta_3_3  CTVAR_1.theta     y     y  0.201182587 0.001015359        0
    #> 19    mu0_1_1    CTVAR_1.mu0 eta_x   mu0  0.006353882 0.109652563         
    #> 20    mu0_2_1    CTVAR_1.mu0 eta_m   mu0 -0.042423917 0.112217050 !       
    #> 21    mu0_3_1    CTVAR_1.mu0 eta_y   mu0  0.130149923 0.100486548 !       
    #> 22 sigma0_1_1 CTVAR_1.sigma0 eta_x eta_x  1.150441191 0.171254549 !      0
    #> 23 sigma0_2_1 CTVAR_1.sigma0 eta_x eta_m  0.413787071 0.132444266 !       
    #> 24 sigma0_2_2 CTVAR_1.sigma0 eta_m eta_m  1.221867391 0.180735288        0
    #> 25 sigma0_3_1 CTVAR_1.sigma0 eta_x eta_y  0.226069494 0.114217258 !       
    #> 26 sigma0_3_2 CTVAR_1.sigma0 eta_m eta_y  0.235392675 0.116761937 !       
    #> 27 sigma0_3_3 CTVAR_1.sigma0 eta_y eta_y  0.962739024 0.143527285        0
    #>    ubound
    #> 1     1.5
    #> 2     1.5
    #> 3     1.5
    #> 4     1.5
    #> 5     1.5
    #> 6     1.5
    #> 7     1.5
    #> 8     1.5
    #> 9     1.5
    #> 10       
    #> 11       
    #> 12       
    #> 13       
    #> 14       
    #> 15       
    #> 16       
    #> 17       
    #> 18       
    #> 19       
    #> 20       
    #> 21       
    #> 22       
    #> 23       
    #> 24       
    #> 25       
    #> 26       
    #> 27       
    #> 
    #> Model Statistics: 
    #>                |  Parameters  |  Degrees of Freedom  |  Fit (-2lnL units)
    #>        Model:             27                 299973              429365.5
    #>    Saturated:             NA                     NA                    NA
    #> Independence:             NA                     NA                    NA
    #> Number of observations/statistics: 1e+05/3e+05
    #> 
    #> Information Criteria: 
    #>       |  df Penalty  |  Parameters Penalty  |  Sample-Size Adjusted
    #> AIC:      -170580.5               429419.5                 429419.5
    #> BIC:     -3024201.3               429676.3                 429590.5
    #> To get additional fit indices, see help(mxRefModels)
    #> timestamp: 2026-02-04 03:28:05 
    #> Wall clock time: 1748.603 secs 
    #> optimizer:  SLSQP 
    #> OpenMx version number: 2.22.10 
    #> Need help?  See help(mxSummary)
    coefs <- coef(fit)
    vcovs <- vcov(fit)

### Extract Matrices from the Fitted Model to use in cTMed

``` r

phi_names <- c(
  "phi_1_1", "phi_2_1", "phi_3_1",
  "phi_1_2", "phi_2_2", "phi_3_2",
  "phi_1_3", "phi_2_3", "phi_3_3"
)
sigma_names <- c(
  "sigma_1_1", "sigma_2_1", "sigma_3_1",
  "sigma_2_1", "sigma_2_2", "sigma_3_2",
  "sigma_3_1", "sigma_3_2", "sigma_3_3"
)
sigma_vech_names <- c(
  "sigma_1_1", "sigma_2_1", "sigma_3_1",
  "sigma_2_2", "sigma_3_2",
  "sigma_3_3"
)
theta_names <- c(
  "phi_1_1", "phi_2_1", "phi_3_1",
  "phi_1_2", "phi_2_2", "phi_3_2",
  "phi_1_3", "phi_2_3", "phi_3_3",
  "sigma_1_1", "sigma_2_1", "sigma_3_1",
  "sigma_2_2", "sigma_3_2",
  "sigma_3_3"
)
phi <- matrix(
  data = coefs[phi_names],
  nrow = 3,
  ncol = 3
)
sigma <- matrix(
  data = coefs[sigma_names],
  nrow = 3,
  ncol = 3
)
theta <- coefs[theta_names]
vcov_phi_vec <- vcovs[phi_names, phi_names]
vcov_sigma_vech <- vcovs[sigma_vech_names, sigma_vech_names]
vcov_theta <- vcovs[theta_names, theta_names]
```

#### Estimated Drift Matrix with Corresponding Sampling Covariance Matrix

``` r

phi
#>            [,1]        [,2]        [,3]
#> [1,] -0.3518208  0.01732532 -0.02380373
#> [2,]  0.7440583 -0.48857995 -0.00993024
#> [3,] -0.4588657  0.72699007 -0.68841648
vcov_phi_vec
#>               phi_1_1       phi_2_1       phi_3_1       phi_1_2       phi_2_2
#> phi_1_1  1.247584e-03  6.204680e-05 -1.797110e-04 -1.044633e-03 -6.054430e-05
#> phi_2_1  6.204680e-05  4.716788e-04  9.538355e-06 -4.016560e-05 -3.991326e-04
#> phi_3_1 -1.797110e-04  9.538355e-06  5.172368e-04  1.447003e-04 -1.731015e-06
#> phi_1_2 -1.044633e-03 -4.016560e-05  1.447003e-04  9.543040e-04  4.669631e-05
#> phi_2_2 -6.054430e-05 -3.991326e-04 -1.731015e-06  4.669631e-05  3.682498e-04
#> phi_3_2  1.577580e-04 -1.561336e-05 -4.381310e-04 -1.419511e-04  7.988600e-06
#> phi_1_3  7.022339e-04  2.238038e-05 -8.777993e-05 -6.650863e-04 -2.765071e-05
#> phi_2_3  4.887109e-05  2.681547e-04 -4.792982e-06 -4.128968e-05 -2.570596e-04
#> phi_3_3 -1.134849e-04  1.591634e-05  2.955074e-04  1.066127e-04 -1.127583e-05
#>               phi_3_2       phi_1_3       phi_2_3       phi_3_3
#> phi_1_1  1.577580e-04  7.022339e-04  4.887109e-05 -1.134849e-04
#> phi_2_1 -1.561336e-05  2.238038e-05  2.681547e-04  1.591634e-05
#> phi_3_1 -4.381310e-04 -8.777993e-05 -4.792982e-06  2.955074e-04
#> phi_1_2 -1.419511e-04 -6.650863e-04 -4.128968e-05  1.066127e-04
#> phi_2_2  7.988600e-06 -2.765071e-05 -2.570596e-04 -1.127583e-05
#> phi_3_2  4.048020e-04  9.139347e-05  1.275627e-06 -2.841744e-04
#> phi_1_3  9.139347e-05  5.576330e-04  3.170146e-05 -8.690637e-05
#> phi_2_3  1.275627e-06  3.170146e-05  2.146221e-04  3.565454e-06
#> phi_3_3 -2.841744e-04 -8.690637e-05  3.565454e-06  2.394910e-04
```

#### Process Noise Covariance Matrix with Corresponding Sampling Covariance Matrix

``` r

sigma
#>             [,1]       [,2]        [,3]
#> [1,]  0.23755283 0.03170294 -0.05361356
#> [2,]  0.03170294 0.07179514  0.01233900
#> [3,] -0.05361356 0.01233900  0.07575179
vcov_sigma_vech
#>               sigma_1_1     sigma_2_1     sigma_3_1     sigma_2_2     sigma_3_2
#> sigma_1_1  4.528560e-05 -7.425746e-07 -4.781448e-06 -5.223248e-07  5.533432e-07
#> sigma_2_1 -7.425746e-07  6.362446e-06 -1.639466e-08 -3.618622e-08 -1.070976e-06
#> sigma_3_1 -4.781448e-06 -1.639466e-08  6.962308e-06  1.520680e-08 -3.684501e-08
#> sigma_2_2 -5.223248e-07 -3.618622e-08  1.520680e-08  3.697522e-06 -4.742080e-08
#> sigma_3_2  5.533432e-07 -1.070976e-06 -3.684501e-08 -4.742080e-08  1.950754e-06
#> sigma_3_3 -2.040071e-08  1.873694e-07 -1.974478e-06 -4.580901e-08 -9.723002e-08
#>               sigma_3_3
#> sigma_1_1 -2.040071e-08
#> sigma_2_1  1.873694e-07
#> sigma_3_1 -1.974478e-06
#> sigma_2_2 -4.580901e-08
#> sigma_3_2 -9.723002e-08
#> sigma_3_3  4.567723e-06
```

#### Estimated Drift Matrix and Process Noise Covariance Matrix with Corresponding Sampling Covariance Matrix

``` r

theta
#>     phi_1_1     phi_2_1     phi_3_1     phi_1_2     phi_2_2     phi_3_2 
#> -0.35182081  0.74405835 -0.45886569  0.01732532 -0.48857995  0.72699007 
#>     phi_1_3     phi_2_3     phi_3_3   sigma_1_1   sigma_2_1   sigma_3_1 
#> -0.02380373 -0.00993024 -0.68841648  0.23755283  0.03170294 -0.05361356 
#>   sigma_2_2   sigma_3_2   sigma_3_3 
#>  0.07179514  0.01233900  0.07575179
vcov_theta
#>                 phi_1_1       phi_2_1       phi_3_1       phi_1_2       phi_2_2
#> phi_1_1    1.247584e-03  6.204680e-05 -1.797110e-04 -1.044633e-03 -6.054430e-05
#> phi_2_1    6.204680e-05  4.716788e-04  9.538355e-06 -4.016560e-05 -3.991326e-04
#> phi_3_1   -1.797110e-04  9.538355e-06  5.172368e-04  1.447003e-04 -1.731015e-06
#> phi_1_2   -1.044633e-03 -4.016560e-05  1.447003e-04  9.543040e-04  4.669631e-05
#> phi_2_2   -6.054430e-05 -3.991326e-04 -1.731015e-06  4.669631e-05  3.682498e-04
#> phi_3_2    1.577580e-04 -1.561336e-05 -4.381310e-04 -1.419511e-04  7.988600e-06
#> phi_1_3    7.022339e-04  2.238038e-05 -8.777993e-05 -6.650863e-04 -2.765071e-05
#> phi_2_3    4.887109e-05  2.681547e-04 -4.792982e-06 -4.128968e-05 -2.570596e-04
#> phi_3_3   -1.134849e-04  1.591634e-05  2.955074e-04  1.066127e-04 -1.127583e-05
#> sigma_1_1 -1.918630e-04 -1.645251e-05  3.037485e-05  1.502686e-04  1.387956e-05
#> sigma_2_1  1.910338e-05 -3.241454e-05 -6.868620e-06 -2.058373e-05  2.412367e-05
#> sigma_3_1  1.058652e-05  1.958428e-06 -3.663026e-05 -4.237886e-06 -1.403297e-06
#> sigma_2_2  3.777621e-06  1.342082e-05  7.952239e-07 -3.444068e-06 -1.419808e-05
#> sigma_3_2 -5.787097e-06  1.830464e-06  7.733913e-06  5.781118e-06  2.325849e-07
#> sigma_3_3  3.816940e-06 -1.075800e-06  2.349552e-06 -4.527518e-06  7.540172e-07
#>                 phi_3_2       phi_1_3       phi_2_3       phi_3_3     sigma_1_1
#> phi_1_1    1.577580e-04  7.022339e-04  4.887109e-05 -1.134849e-04 -1.918630e-04
#> phi_2_1   -1.561336e-05  2.238038e-05  2.681547e-04  1.591634e-05 -1.645251e-05
#> phi_3_1   -4.381310e-04 -8.777993e-05 -4.792982e-06  2.955074e-04  3.037485e-05
#> phi_1_2   -1.419511e-04 -6.650863e-04 -4.128968e-05  1.066127e-04  1.502686e-04
#> phi_2_2    7.988600e-06 -2.765071e-05 -2.570596e-04 -1.127583e-05  1.387956e-05
#> phi_3_2    4.048020e-04  9.139347e-05  1.275627e-06 -2.841744e-04 -2.484591e-05
#> phi_1_3    9.139347e-05  5.576330e-04  3.170146e-05 -8.690637e-05 -9.304952e-05
#> phi_2_3    1.275627e-06  3.170146e-05  2.146221e-04  3.565454e-06 -9.558393e-06
#> phi_3_3   -2.841744e-04 -8.690637e-05  3.565454e-06  2.394910e-04  1.640237e-05
#> sigma_1_1 -2.484591e-05 -9.304952e-05 -9.558393e-06  1.640237e-05  4.528560e-05
#> sigma_2_1  6.855446e-06  1.425503e-05 -1.402873e-05 -4.894158e-06 -7.425746e-07
#> sigma_3_1  2.739421e-05 -4.219143e-06  3.921594e-07 -1.508627e-05 -4.781448e-06
#> sigma_2_2 -1.122974e-06  2.318940e-06  9.347026e-06  8.813382e-07 -5.223248e-07
#> sigma_3_2 -8.028180e-06 -4.213819e-06 -2.603417e-06  5.173124e-06  5.533432e-07
#> sigma_3_3  1.745357e-06  5.208954e-06 -9.585209e-08 -6.774855e-06 -2.040071e-08
#>               sigma_2_1     sigma_3_1     sigma_2_2     sigma_3_2     sigma_3_3
#> phi_1_1    1.910338e-05  1.058652e-05  3.777621e-06 -5.787097e-06  3.816940e-06
#> phi_2_1   -3.241454e-05  1.958428e-06  1.342082e-05  1.830464e-06 -1.075800e-06
#> phi_3_1   -6.868620e-06 -3.663026e-05  7.952239e-07  7.733913e-06  2.349552e-06
#> phi_1_2   -2.058373e-05 -4.237886e-06 -3.444068e-06  5.781118e-06 -4.527518e-06
#> phi_2_2    2.412367e-05 -1.403297e-06 -1.419808e-05  2.325849e-07  7.540172e-07
#> phi_3_2    6.855446e-06  2.739421e-05 -1.122974e-06 -8.028180e-06  1.745357e-06
#> phi_1_3    1.425503e-05 -4.219143e-06  2.318940e-06 -4.213819e-06  5.208954e-06
#> phi_2_3   -1.402873e-05  3.921594e-07  9.347026e-06 -2.603417e-06 -9.585209e-08
#> phi_3_3   -4.894158e-06 -1.508627e-05  8.813382e-07  5.173124e-06 -6.774855e-06
#> sigma_1_1 -7.425746e-07 -4.781448e-06 -5.223248e-07  5.533432e-07 -2.040071e-08
#> sigma_2_1  6.362446e-06 -1.639466e-08 -3.618622e-08 -1.070976e-06  1.873694e-07
#> sigma_3_1 -1.639466e-08  6.962308e-06  1.520680e-08 -3.684501e-08 -1.974478e-06
#> sigma_2_2 -3.618622e-08  1.520680e-08  3.697522e-06 -4.742080e-08 -4.580901e-08
#> sigma_3_2 -1.070976e-06 -3.684501e-08 -4.742080e-08  1.950754e-06 -9.723002e-08
#> sigma_3_3  1.873694e-07 -1.974478e-06 -4.580901e-08 -9.723002e-08  4.567723e-06
```

## References

Hunter, M. D. (2017). State space modeling in an open source, modular,
structural equation modeling environment. *Structural Equation Modeling:
A Multidisciplinary Journal*, *25*(2), 307–324.
<https://doi.org/10.1080/10705511.2017.1369354>

Neale, M. C., Hunter, M. D., Pritikin, J. N., Zahery, M., Brick, T. R.,
Kirkpatrick, R. M., Estabrook, R., Bates, T. C., Maes, H. H., & Boker,
S. M. (2015). OpenMx 2.0: Extended structural equation and statistical
modeling. *Psychometrika*, *81*(2), 535–549.
<https://doi.org/10.1007/s11336-014-9435-8>

Pesigan, I. J. A., Russell, M. A., & Chow, S.-M. (2025). Inferences and
effect sizes for direct, indirect, and total effects in continuous-time
mediation models. *Psychological Methods*.
<https://doi.org/10.1037/met0000779>

R Core Team. (2024). *R: A language and environment for statistical
computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>

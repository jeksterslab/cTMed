# Delta Method Sampling Variance-Covariance Matrix for the Standardized Direct Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function computes the delta method sampling variance-covariance
matrix for the standardized direct effect centrality over a specific
time interval \\\Delta t\\ or a range of time intervals using the
first-order stochastic differential equation model's drift matrix
\\\boldsymbol{\Phi}\\ and process noise covariance matrix
\\\boldsymbol{\Sigma}\\.

## Usage

``` r
DeltaDirectCentralStd(
  phi,
  sigma,
  vcov_theta,
  delta_t,
  sigma_diag = FALSE,
  ncores = NULL,
  tol = 0.001
)
```

## Arguments

- phi:

  Numeric matrix. The drift matrix (\\\boldsymbol{\Phi}\\). `phi` should
  have row and column names pertaining to the variables in the system.

- sigma:

  Numeric matrix. The process noise covariance matrix
  (\\\boldsymbol{\Sigma}\\).

- vcov_theta:

  Numeric matrix. The sampling variance-covariance matrix of
  \\\mathrm{vec} \left( \boldsymbol{\Phi} \right)\\ and \\\mathrm{vech}
  \left( \boldsymbol{\Sigma} \right)\\

- delta_t:

  Vector of positive numbers. Time interval (\\\Delta t\\).

- sigma_diag:

  Logical. If `sigma_diag = TRUE`, treat \\\boldsymbol{\Sigma}\\ as a
  diagonal matrix.

- ncores:

  Positive integer. Number of cores to use. If `ncores = NULL`, use a
  single core. Consider using multiple cores when the length of
  `delta_t` is long.

- tol:

  Numeric. Smallest possible time interval to allow.

## Value

Returns an object of class `ctmeddelta` which is a list with the
following elements:

- call:

  Function call.

- args:

  Function arguments.

- fun:

  Function used ("DeltaDirectCentralStd").

- output:

  A list of length `length(delta_t)`.

Each element in the `output` list has the following elements:

- delta_t:

  Time interval.

- jacobian:

  Jacobian matrix.

- est:

  Estimated standardized direct effect centrality.

- vcov:

  Sampling variance-covariance matrix of estimated standardized direct
  effect centrality.

## Details

See
[`DirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/DirectCentralStd.md)
for more details.

### Delta Method

Let \\\boldsymbol{\theta}\\ be a vector that combines \\\mathrm{vec}
\left( \boldsymbol{\Phi} \right)\\, that is, the elements of the
\\\boldsymbol{\Phi}\\ matrix in vector form sorted column-wise and
\\\mathrm{vech} \left( \boldsymbol{\Sigma} \right)\\, that is, the
unique elements of the \\\boldsymbol{\Sigma}\\ matrix in vector form
sorted column-wise. Let \\\hat{\boldsymbol{\theta}}\\ be a vector that
combines \\\mathrm{vec} \left( \hat{\boldsymbol{\Phi}} \right)\\ and
\\\mathrm{vech} \left( \hat{\boldsymbol{\Sigma}} \right)\\. By the
multivariate central limit theory, the function \\\mathbf{g}\\ using
\\\hat{\boldsymbol{\theta}}\\ as input can be expressed as:

\$\$ \sqrt{n} \left( \mathbf{g} \left( \hat{\boldsymbol{\theta}}
\right) - \mathbf{g} \left( \boldsymbol{\theta} \right) \right)
\xrightarrow\[\]{ \mathrm{D} } \mathcal{N} \left( 0, \mathbf{J}
\boldsymbol{\Gamma} \mathbf{J}^{\prime} \right) \$\$

where \\\mathbf{J}\\ is the matrix of first-order derivatives of the
function \\\mathbf{g}\\ with respect to the elements of
\\\boldsymbol{\theta}\\ and \\\boldsymbol{\Gamma}\\ is the asymptotic
variance-covariance matrix of \\\hat{\boldsymbol{\theta}}\\.

From the former, we can derive the distribution of \\\mathbf{g} \left(
\hat{\boldsymbol{\theta}} \right)\\ as follows:

\$\$ \mathbf{g} \left( \hat{\boldsymbol{\theta}} \right) \approx
\mathcal{N} \left( \mathbf{g} \left( \boldsymbol{\theta} \right) ,
n^{-1} \mathbf{J} \boldsymbol{\Gamma} \mathbf{J}^{\prime} \right) \$\$

The uncertainty associated with the estimator \\\mathbf{g} \left(
\hat{\boldsymbol{\theta}} \right)\\ is, therefore, given by \\n^{-1}
\mathbf{J} \boldsymbol{\Gamma} \mathbf{J}^{\prime}\\ . When
\\\boldsymbol{\Gamma}\\ is unknown, by substitution, we can use the
estimated sampling variance-covariance matrix of
\\\hat{\boldsymbol{\theta}}\\, that is, \\\hat{\mathbb{V}} \left(
\hat{\boldsymbol{\theta}} \right)\\ for \\n^{-1} \boldsymbol{\Gamma}\\.
Therefore, the sampling variance-covariance matrix of \\\mathbf{g}
\left( \hat{\boldsymbol{\theta}} \right)\\ is given by

\$\$ \mathbf{g} \left( \hat{\boldsymbol{\theta}} \right) \approx
\mathcal{N} \left( \mathbf{g} \left( \boldsymbol{\theta} \right) ,
\mathbf{J} \hat{\mathbb{V}} \left( \hat{\boldsymbol{\theta}} \right)
\mathbf{J}^{\prime} \right) . \$\$

## References

Bollen, K. A. (1987). Total, direct, and indirect effects in structural
equation models. Sociological Methodology, 17, 37.
[doi:10.2307/271028](https://doi.org/10.2307/271028)

Deboeck, P. R., & Preacher, K. J. (2015). No need to be discrete: A
method for continuous time mediation analysis. Structural Equation
Modeling: A Multidisciplinary Journal, 23 (1), 61-75.
[doi:10.1080/10705511.2014.973960](https://doi.org/10.1080/10705511.2014.973960)

Pesigan, I. J. A., Russell, M. A., & Chow, S.-M. (2025). Inferences and
effect sizes for direct, indirect, and total effects in continuous-time
mediation models. Psychological Methods.
[doi:10.1037/met0000779](https://doi.org/10.1037/met0000779)

Ryan, O., & Hamaker, E. L. (2021). Time to intervene: A continuous-time
approach to network analysis and centrality. Psychometrika, 87 (1),
214-252.
[doi:10.1007/s11336-021-09767-0](https://doi.org/10.1007/s11336-021-09767-0)

## See also

Other Continuous-Time Mediation Functions:
[`BootBeta()`](https://github.com/jeksterslab/cTMed/reference/BootBeta.md),
[`BootBetaStd()`](https://github.com/jeksterslab/cTMed/reference/BootBetaStd.md),
[`BootDirectCentral()`](https://github.com/jeksterslab/cTMed/reference/BootDirectCentral.md),
[`BootDirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/BootDirectCentralStd.md),
[`BootIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/BootIndirectCentral.md),
[`BootIndirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/BootIndirectCentralStd.md),
[`BootMed()`](https://github.com/jeksterslab/cTMed/reference/BootMed.md),
[`BootMedStd()`](https://github.com/jeksterslab/cTMed/reference/BootMedStd.md),
[`BootTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/BootTotalCentral.md),
[`BootTotalCentralStd()`](https://github.com/jeksterslab/cTMed/reference/BootTotalCentralStd.md),
[`DeltaBeta()`](https://github.com/jeksterslab/cTMed/reference/DeltaBeta.md),
[`DeltaBetaStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaBetaStd.md),
[`DeltaDirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaDirectCentral.md),
[`DeltaIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaIndirectCentral.md),
[`DeltaMed()`](https://github.com/jeksterslab/cTMed/reference/DeltaMed.md),
[`DeltaMedStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaMedStd.md),
[`DeltaTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaTotalCentral.md),
[`DeltaTotalCentralStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaTotalCentralStd.md),
[`Direct()`](https://github.com/jeksterslab/cTMed/reference/Direct.md),
[`DirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DirectCentral.md),
[`DirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/DirectCentralStd.md),
[`DirectStd()`](https://github.com/jeksterslab/cTMed/reference/DirectStd.md),
[`Indirect()`](https://github.com/jeksterslab/cTMed/reference/Indirect.md),
[`IndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/IndirectCentral.md),
[`IndirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/IndirectCentralStd.md),
[`IndirectStd()`](https://github.com/jeksterslab/cTMed/reference/IndirectStd.md),
[`MCBeta()`](https://github.com/jeksterslab/cTMed/reference/MCBeta.md),
[`MCBetaStd()`](https://github.com/jeksterslab/cTMed/reference/MCBetaStd.md),
[`MCDirectCentral()`](https://github.com/jeksterslab/cTMed/reference/MCDirectCentral.md),
[`MCDirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/MCDirectCentralStd.md),
[`MCIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/MCIndirectCentral.md),
[`MCIndirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/MCIndirectCentralStd.md),
[`MCMed()`](https://github.com/jeksterslab/cTMed/reference/MCMed.md),
[`MCMedStd()`](https://github.com/jeksterslab/cTMed/reference/MCMedStd.md),
[`MCPhi()`](https://github.com/jeksterslab/cTMed/reference/MCPhi.md),
[`MCPhiSigma()`](https://github.com/jeksterslab/cTMed/reference/MCPhiSigma.md),
[`MCTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/MCTotalCentral.md),
[`MCTotalCentralStd()`](https://github.com/jeksterslab/cTMed/reference/MCTotalCentralStd.md),
[`Med()`](https://github.com/jeksterslab/cTMed/reference/Med.md),
[`MedStd()`](https://github.com/jeksterslab/cTMed/reference/MedStd.md),
[`PosteriorBeta()`](https://github.com/jeksterslab/cTMed/reference/PosteriorBeta.md),
[`PosteriorBetaStd()`](https://github.com/jeksterslab/cTMed/reference/PosteriorBetaStd.md),
[`PosteriorDirectCentral()`](https://github.com/jeksterslab/cTMed/reference/PosteriorDirectCentral.md),
[`PosteriorDirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/PosteriorDirectCentralStd.md),
[`PosteriorIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/PosteriorIndirectCentral.md),
[`PosteriorIndirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/PosteriorIndirectCentralStd.md),
[`PosteriorMed()`](https://github.com/jeksterslab/cTMed/reference/PosteriorMed.md),
[`PosteriorMedStd()`](https://github.com/jeksterslab/cTMed/reference/PosteriorMedStd.md),
[`PosteriorTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/PosteriorTotalCentral.md),
[`PosteriorTotalCentralStd()`](https://github.com/jeksterslab/cTMed/reference/PosteriorTotalCentralStd.md),
[`Total()`](https://github.com/jeksterslab/cTMed/reference/Total.md),
[`TotalCentral()`](https://github.com/jeksterslab/cTMed/reference/TotalCentral.md),
[`TotalCentralStd()`](https://github.com/jeksterslab/cTMed/reference/TotalCentralStd.md),
[`TotalStd()`](https://github.com/jeksterslab/cTMed/reference/TotalStd.md),
[`Trajectory()`](https://github.com/jeksterslab/cTMed/reference/Trajectory.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
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
vcov_theta <- matrix(
  data = c(
    0.00843, 0.00040, -0.00151, -0.00600, -0.00033,
    0.00110, 0.00324, 0.00020, -0.00061, -0.00115,
    0.00011, 0.00015, 0.00001, -0.00002, -0.00001,
    0.00040, 0.00374, 0.00016, -0.00022, -0.00273,
    -0.00016, 0.00009, 0.00150, 0.00012, -0.00010,
    -0.00026, 0.00002, 0.00012, 0.00004, -0.00001,
    -0.00151, 0.00016, 0.00389, 0.00103, -0.00007,
    -0.00283, -0.00050, 0.00000, 0.00156, 0.00021,
    -0.00005, -0.00031, 0.00001, 0.00007, 0.00006,
    -0.00600, -0.00022, 0.00103, 0.00644, 0.00031,
    -0.00119, -0.00374, -0.00021, 0.00070, 0.00064,
    -0.00015, -0.00005, 0.00000, 0.00003, -0.00001,
    -0.00033, -0.00273, -0.00007, 0.00031, 0.00287,
    0.00013, -0.00014, -0.00170, -0.00012, 0.00006,
    0.00014, -0.00001, -0.00015, 0.00000, 0.00001,
    0.00110, -0.00016, -0.00283, -0.00119, 0.00013,
    0.00297, 0.00063, -0.00004, -0.00177, -0.00013,
    0.00005, 0.00017, -0.00002, -0.00008, 0.00001,
    0.00324, 0.00009, -0.00050, -0.00374, -0.00014,
    0.00063, 0.00495, 0.00024, -0.00093, -0.00020,
    0.00006, -0.00010, 0.00000, -0.00001, 0.00004,
    0.00020, 0.00150, 0.00000, -0.00021, -0.00170,
    -0.00004, 0.00024, 0.00214, 0.00012, -0.00002,
    -0.00004, 0.00000, 0.00006, -0.00005, -0.00001,
    -0.00061, 0.00012, 0.00156, 0.00070, -0.00012,
    -0.00177, -0.00093, 0.00012, 0.00223, 0.00004,
    -0.00002, -0.00003, 0.00001, 0.00003, -0.00013,
    -0.00115, -0.00010, 0.00021, 0.00064, 0.00006,
    -0.00013, -0.00020, -0.00002, 0.00004, 0.00057,
    0.00001, -0.00009, 0.00000, 0.00000, 0.00001,
    0.00011, -0.00026, -0.00005, -0.00015, 0.00014,
    0.00005, 0.00006, -0.00004, -0.00002, 0.00001,
    0.00012, 0.00001, 0.00000, -0.00002, 0.00000,
    0.00015, 0.00002, -0.00031, -0.00005, -0.00001,
    0.00017, -0.00010, 0.00000, -0.00003, -0.00009,
    0.00001, 0.00014, 0.00000, 0.00000, -0.00005,
    0.00001, 0.00012, 0.00001, 0.00000, -0.00015,
    -0.00002, 0.00000, 0.00006, 0.00001, 0.00000,
    0.00000, 0.00000, 0.00010, 0.00001, 0.00000,
    -0.00002, 0.00004, 0.00007, 0.00003, 0.00000,
    -0.00008, -0.00001, -0.00005, 0.00003, 0.00000,
    -0.00002, 0.00000, 0.00001, 0.00005, 0.00001,
    -0.00001, -0.00001, 0.00006, -0.00001, 0.00001,
    0.00001, 0.00004, -0.00001, -0.00013, 0.00001,
    0.00000, -0.00005, 0.00000, 0.00001, 0.00012
  ),
  nrow = 15
)

# Specific time interval ----------------------------------------------------
DeltaDirectCentralStd(
  phi = phi,
  sigma = sigma,
  vcov_theta = vcov_theta,
  delta_t = 1
)
#> Call:
#> DeltaDirectCentralStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1)
#> 
#> Direct Effect Centrality
#>   variable interval     est     se       z p    2.5%   97.5%
#> 1        x        1  0.5494 0.0351 15.6507 0  0.4806  0.6182
#> 2        m        1 -0.2858 0.0558 -5.1247 0 -0.3951 -0.1765
#> 3        y        1  0.3888 0.0570  6.8252 0  0.2772  0.5005

# Range of time intervals ---------------------------------------------------
delta <- DeltaDirectCentralStd(
  phi = phi,
  sigma = sigma,
  vcov_theta = vcov_theta,
  delta_t = 1:5
)
plot(delta)




# Methods -------------------------------------------------------------------
# DeltaDirectCentralStd has a number of methods including
# print, summary, confint, and plot
print(delta)
#> Call:
#> DeltaDirectCentralStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se       z      p    2.5%   97.5%
#> 1         x        1  0.5494 0.0351 15.6507 0.0000  0.4806  0.6182
#> 2         m        1 -0.2858 0.0558 -5.1247 0.0000 -0.3951 -0.1765
#> 3         y        1  0.3888 0.0570  6.8252 0.0000  0.2772  0.5005
#> 4         x        2  0.6044 0.0357 16.9314 0.0000  0.5344  0.6743
#> 5         m        2 -0.3429 0.0723 -4.7449 0.0000 -0.4845 -0.2013
#> 6         y        2  0.5053 0.0702  7.2030 0.0000  0.3678  0.6428
#> 7         x        3  0.4999 0.0356 14.0449 0.0000  0.4302  0.5697
#> 8         m        3 -0.3114 0.0756 -4.1217 0.0000 -0.4595 -0.1633
#> 9         y        3  0.4936 0.0779  6.3391 0.0000  0.3410  0.6462
#> 10        x        4  0.3686 0.0355 10.3872 0.0000  0.2991  0.4382
#> 11        m        4 -0.2537 0.0728 -3.4853 0.0005 -0.3963 -0.1110
#> 12        y        4  0.4293 0.0853  5.0329 0.0000  0.2621  0.5965
#> 13        x        5  0.2555 0.0334  7.6489 0.0000  0.1900  0.3210
#> 14        m        5 -0.1954 0.0665 -2.9371 0.0033 -0.3258 -0.0650
#> 15        y        5  0.3508 0.0902  3.8874 0.0001  0.1739  0.5276
summary(delta)
#> Call:
#> DeltaDirectCentralStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se       z      p    2.5%   97.5%
#> 1         x        1  0.5494 0.0351 15.6507 0.0000  0.4806  0.6182
#> 2         m        1 -0.2858 0.0558 -5.1247 0.0000 -0.3951 -0.1765
#> 3         y        1  0.3888 0.0570  6.8252 0.0000  0.2772  0.5005
#> 4         x        2  0.6044 0.0357 16.9314 0.0000  0.5344  0.6743
#> 5         m        2 -0.3429 0.0723 -4.7449 0.0000 -0.4845 -0.2013
#> 6         y        2  0.5053 0.0702  7.2030 0.0000  0.3678  0.6428
#> 7         x        3  0.4999 0.0356 14.0449 0.0000  0.4302  0.5697
#> 8         m        3 -0.3114 0.0756 -4.1217 0.0000 -0.4595 -0.1633
#> 9         y        3  0.4936 0.0779  6.3391 0.0000  0.3410  0.6462
#> 10        x        4  0.3686 0.0355 10.3872 0.0000  0.2991  0.4382
#> 11        m        4 -0.2537 0.0728 -3.4853 0.0005 -0.3963 -0.1110
#> 12        y        4  0.4293 0.0853  5.0329 0.0000  0.2621  0.5965
#> 13        x        5  0.2555 0.0334  7.6489 0.0000  0.1900  0.3210
#> 14        m        5 -0.1954 0.0665 -2.9371 0.0033 -0.3258 -0.0650
#> 15        y        5  0.3508 0.0902  3.8874 0.0001  0.1739  0.5276
confint(delta, level = 0.95)
#>    variable interval      2.5 %      97.5 %
#> 1         x        1  0.4806248  0.61823692
#> 2         m        1 -0.3950795 -0.17648410
#> 3         y        1  0.2771671  0.50048203
#> 4         x        2  0.5343971  0.67431668
#> 5         m        2 -0.4845323 -0.20125600
#> 6         y        2  0.3678369  0.64284875
#> 7         x        3  0.4301811  0.56971701
#> 8         m        3 -0.4594985 -0.16333027
#> 9         y        3  0.3409526  0.64615352
#> 10        x        4  0.2990703  0.43818241
#> 11        m        4 -0.3963017 -0.11101379
#> 12        y        4  0.2621272  0.59650683
#> 13        x        5  0.1900281  0.32096563
#> 14        m        5 -0.3257505 -0.06499861
#> 15        y        5  0.1739243  0.52763726
plot(delta)



```

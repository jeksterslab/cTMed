# Posterior Distribution of the Standardized Indirect Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function generates a posterior distribution of the standardized
indirect effect centrality over a specific time interval \\\Delta t\\ or
a range of time intervals using the posterior distribution of the
first-order stochastic differential equation model drift matrix
\\\boldsymbol{\Phi}\\ and process noise covariance matrix
\\\boldsymbol{\Sigma}\\.

## Usage

``` r
PosteriorIndirectCentralStd(phi, sigma, delta_t, ncores = NULL, tol = 0.001)
```

## Arguments

- phi:

  List of numeric matrices. Each element of the list is a sample from
  the posterior distribution of the drift matrix
  (\\\boldsymbol{\Phi}\\). Each matrix should have row and column names
  pertaining to the variables in the system.

- sigma:

  List of numeric matrices. Each element is a posterior draw of the
  diffusion covariance matrix.

- delta_t:

  Numeric. Time interval (\\\Delta t\\).

- ncores:

  Positive integer. Number of cores to use. If `ncores = NULL`, use a
  single core. Consider using multiple cores when number of replications
  `R` is a large value.

- tol:

  Numeric. Smallest possible time interval to allow.

## Value

Returns an object of class `ctmedmc` which is a list with the following
elements:

- call:

  Function call.

- args:

  Function arguments.

- fun:

  Function used ("PosteriorIndirectCentralStd").

- output:

  A list of length `length(delta_t)`.

Each element in the `output` list has the following elements:

- est:

  Mean of the posterior distribution of the standardized indirect effect
  centrality.

- thetahatstar:

  Posterior distribution of the standardized indirect effect centrality
  measure.

## Details

See
[`IndirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/IndirectCentralStd.md)
for more details.

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
[`DeltaDirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaDirectCentralStd.md),
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
set.seed(42)
phi <- matrix(
  data = c(
    -0.357, 0.771, -0.450,
    0.000, -0.511, 0.729,
    0.000, 0.000, -0.693
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

input <- MCPhiSigma(
  phi = phi,
  sigma = sigma,
  vcov_theta = 0.001 * diag(15),
  R = 100L,
  seed = 42
)$output

phi <- lapply(
  X = input,
  FUN = function(x) {
    x[[1]]
  }
)

sigma <- lapply(
  X = input,
  FUN = function(x) {
    x[[2]]
  }
)

# Specific time interval ----------------------------------------------------
PosteriorIndirectCentralStd(
  phi = phi,
  sigma = sigma,
  delta_t = 1
)
#> Call:
#> PosteriorIndirectCentralStd(phi = phi, sigma = sigma, delta_t = 1)
#> 
#> Indirect Effect Centrality
#>   variable interval     est     se   R    2.5%  97.5%
#> 1        x        1 -0.0006 0.0049 100 -0.0100 0.0087
#> 2        m        1  0.1781 0.0191 100  0.1452 0.2131
#> 3        y        1 -0.0003 0.0089 100 -0.0167 0.0151

# Range of time intervals ---------------------------------------------------
posterior <- PosteriorIndirectCentralStd(
  phi = phi,
  sigma = sigma,
  delta_t = 1:5
)

# Methods -------------------------------------------------------------------
# PosteriorIndirectCentralStd has a number of methods including
# print, summary, confint, and plot
print(posterior)
#> Call:
#> PosteriorIndirectCentralStd(phi = phi, sigma = sigma, delta_t = 1:5)
#> 
#> Indirect Effect Centrality
#>    variable interval     est     se   R    2.5%  97.5%
#> 1         x        1 -0.0006 0.0049 100 -0.0100 0.0087
#> 2         m        1  0.1781 0.0191 100  0.1452 0.2131
#> 3         y        1 -0.0003 0.0089 100 -0.0167 0.0151
#> 4         x        2 -0.0005 0.0106 100 -0.0198 0.0161
#> 5         m        2  0.4260 0.0458 100  0.3531 0.5071
#> 6         y        2  0.0000 0.0208 100 -0.0379 0.0319
#> 7         x        3  0.0006 0.0192 100 -0.0355 0.0307
#> 8         m        3  0.5768 0.0639 100  0.4832 0.6934
#> 9         y        3  0.0011 0.0301 100 -0.0520 0.0463
#> 10        x        4  0.0023 0.0304 100 -0.0543 0.0557
#> 11        m        4  0.6212 0.0727 100  0.5111 0.7563
#> 12        y        4  0.0025 0.0373 100 -0.0645 0.0608
#> 13        x        5  0.0040 0.0408 100 -0.0715 0.0788
#> 14        m        5  0.5920 0.0749 100  0.4804 0.7388
#> 15        y        5  0.0038 0.0428 100 -0.0727 0.0732
summary(posterior)
#> Call:
#> PosteriorIndirectCentralStd(phi = phi, sigma = sigma, delta_t = 1:5)
#> 
#> Indirect Effect Centrality
#>    variable interval     est     se   R    2.5%  97.5%
#> 1         x        1 -0.0006 0.0049 100 -0.0100 0.0087
#> 2         m        1  0.1781 0.0191 100  0.1452 0.2131
#> 3         y        1 -0.0003 0.0089 100 -0.0167 0.0151
#> 4         x        2 -0.0005 0.0106 100 -0.0198 0.0161
#> 5         m        2  0.4260 0.0458 100  0.3531 0.5071
#> 6         y        2  0.0000 0.0208 100 -0.0379 0.0319
#> 7         x        3  0.0006 0.0192 100 -0.0355 0.0307
#> 8         m        3  0.5768 0.0639 100  0.4832 0.6934
#> 9         y        3  0.0011 0.0301 100 -0.0520 0.0463
#> 10        x        4  0.0023 0.0304 100 -0.0543 0.0557
#> 11        m        4  0.6212 0.0727 100  0.5111 0.7563
#> 12        y        4  0.0025 0.0373 100 -0.0645 0.0608
#> 13        x        5  0.0040 0.0408 100 -0.0715 0.0788
#> 14        m        5  0.5920 0.0749 100  0.4804 0.7388
#> 15        y        5  0.0038 0.0428 100 -0.0727 0.0732
confint(posterior, level = 0.95)
#>    variable interval        2.5 %      97.5 %
#> 1         x        1 -0.009991491 0.008712481
#> 2         m        1  0.145235035 0.213100578
#> 3         y        1 -0.016687532 0.015126158
#> 4         x        2 -0.019842453 0.016098764
#> 5         m        2  0.353137765 0.507054970
#> 6         y        2 -0.037873960 0.031930954
#> 7         x        3 -0.035536661 0.030662054
#> 8         m        3  0.483240371 0.693426107
#> 9         y        3 -0.052022732 0.046317996
#> 10        x        4 -0.054342499 0.055743710
#> 11        m        4  0.511107527 0.756309068
#> 12        y        4 -0.064527324 0.060787523
#> 13        x        5 -0.071513516 0.078787667
#> 14        m        5  0.480385059 0.738783716
#> 15        y        5 -0.072699808 0.073208181
plot(posterior)



```

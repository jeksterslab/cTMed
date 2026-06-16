# Posterior Distribution of the Standardized Direct Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function generates a posterior distribution of the standardized
direct effect centrality over a specific time interval \\\Delta t\\ or a
range of time intervals using the posterior distribution of the
first-order stochastic differential equation model drift matrix
\\\boldsymbol{\Phi}\\ and process noise covariance matrix
\\\boldsymbol{\Sigma}\\.

## Usage

``` r
PosteriorDirectCentralStd(phi, sigma, delta_t, ncores = NULL, tol = 0.001)
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

  Function used ("PosteriorDirectCentralStd").

- output:

  A list of length `length(delta_t)`.

Each element in the `output` list has the following elements:

- est:

  Mean of the posterior distribution of the standardized direct effect
  centrality.

- thetahatstar:

  Posterior distribution of the standardized direct effect centrality
  measure.

## Details

See
[`DirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/DirectCentralStd.md)
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
PosteriorDirectCentralStd(
  phi = phi,
  sigma = sigma,
  delta_t = 1
)
#> Call:
#> PosteriorDirectCentralStd(phi = phi, sigma = sigma, delta_t = 1)
#> 
#> Direct Effect Centrality
#>   variable interval     est     se   R    2.5%   97.5%
#> 1        x        1  0.5575 0.0420 100  0.4731  0.6458
#> 2        m        1 -0.2868 0.0508 100 -0.4038 -0.1999
#> 3        y        1  0.3897 0.0323 100  0.3248  0.4410

# Range of time intervals ---------------------------------------------------
posterior <- PosteriorDirectCentralStd(
  phi = phi,
  sigma = sigma,
  delta_t = 1:5
)

# Methods -------------------------------------------------------------------
# PosteriorDirectCentralStd has a number of methods including
# print, summary, confint, and plot
print(posterior)
#> Call:
#> PosteriorDirectCentralStd(phi = phi, sigma = sigma, delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se   R    2.5%   97.5%
#> 1         x        1  0.5575 0.0420 100  0.4731  0.6458
#> 2         m        1 -0.2868 0.0508 100 -0.4038 -0.1999
#> 3         y        1  0.3897 0.0323 100  0.3248  0.4410
#> 4         x        2  0.6157 0.0491 100  0.5141  0.7142
#> 5         m        2 -0.3439 0.0644 100 -0.4888 -0.2367
#> 6         y        2  0.5067 0.0452 100  0.4122  0.5850
#> 7         x        3  0.5127 0.0479 100  0.4195  0.6052
#> 8         m        3 -0.3124 0.0636 100 -0.4457 -0.2107
#> 9         y        3  0.4963 0.0529 100  0.3962  0.6008
#> 10        x        4  0.3816 0.0445 100  0.3007  0.4673
#> 11        m        4 -0.2548 0.0576 100 -0.3667 -0.1671
#> 12        y        4  0.4340 0.0579 100  0.3305  0.5501
#> 13        x        5  0.2678 0.0397 100  0.1971  0.3473
#> 14        m        5 -0.1968 0.0502 100 -0.2940 -0.1211
#> 15        y        5  0.3575 0.0602 100  0.2558  0.4806
summary(posterior)
#> Call:
#> PosteriorDirectCentralStd(phi = phi, sigma = sigma, delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se   R    2.5%   97.5%
#> 1         x        1  0.5575 0.0420 100  0.4731  0.6458
#> 2         m        1 -0.2868 0.0508 100 -0.4038 -0.1999
#> 3         y        1  0.3897 0.0323 100  0.3248  0.4410
#> 4         x        2  0.6157 0.0491 100  0.5141  0.7142
#> 5         m        2 -0.3439 0.0644 100 -0.4888 -0.2367
#> 6         y        2  0.5067 0.0452 100  0.4122  0.5850
#> 7         x        3  0.5127 0.0479 100  0.4195  0.6052
#> 8         m        3 -0.3124 0.0636 100 -0.4457 -0.2107
#> 9         y        3  0.4963 0.0529 100  0.3962  0.6008
#> 10        x        4  0.3816 0.0445 100  0.3007  0.4673
#> 11        m        4 -0.2548 0.0576 100 -0.3667 -0.1671
#> 12        y        4  0.4340 0.0579 100  0.3305  0.5501
#> 13        x        5  0.2678 0.0397 100  0.1971  0.3473
#> 14        m        5 -0.1968 0.0502 100 -0.2940 -0.1211
#> 15        y        5  0.3575 0.0602 100  0.2558  0.4806
confint(posterior, level = 0.95)
#>    variable interval      2.5 %     97.5 %
#> 1         x        1  0.4730964  0.6458223
#> 2         m        1 -0.4038379 -0.1998902
#> 3         y        1  0.3248434  0.4410491
#> 4         x        2  0.5141040  0.7141747
#> 5         m        2 -0.4887537 -0.2367458
#> 6         y        2  0.4122335  0.5850121
#> 7         x        3  0.4194614  0.6052307
#> 8         m        3 -0.4457488 -0.2107098
#> 9         y        3  0.3962477  0.6008256
#> 10        x        4  0.3007393  0.4673146
#> 11        m        4 -0.3667067 -0.1670797
#> 12        y        4  0.3304900  0.5500645
#> 13        x        5  0.1970597  0.3472963
#> 14        m        5 -0.2939719 -0.1211268
#> 15        y        5  0.2557957  0.4805656
plot(posterior)



```

# Posterior Distribution of the Total Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function generates a posterior distribution of the total effect
centrality over a specific time interval \\\Delta t\\ or a range of time
intervals using the posterior distribution of the first-order stochastic
differential equation model drift matrix \\\boldsymbol{\Phi}\\.

## Usage

``` r
PosteriorTotalCentral(phi, delta_t, ncores = NULL, tol = 0.001)
```

## Arguments

- phi:

  List of numeric matrices. Each element of the list is a sample from
  the posterior distribution of the drift matrix
  (\\\boldsymbol{\Phi}\\). Each matrix should have row and column names
  pertaining to the variables in the system.

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

  Function used ("PosteriorTotalCentral").

- output:

  A list of length `length(delta_t)`.

Each element in the `output` list has the following elements:

- est:

  Mean of the posterior distribution of the total, direct, and indirect
  effects.

- thetahatstar:

  Posterior distribution of the total effect centrality measure.

## Details

See
[`TotalCentral()`](https://github.com/jeksterslab/cTMed/reference/TotalCentral.md)
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
[`PosteriorIndirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/PosteriorIndirectCentralStd.md),
[`PosteriorMed()`](https://github.com/jeksterslab/cTMed/reference/PosteriorMed.md),
[`PosteriorMedStd()`](https://github.com/jeksterslab/cTMed/reference/PosteriorMedStd.md),
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
vcov_phi_vec <- matrix(
  data = c(
    0.00843, 0.00040, -0.00151,
    -0.00600, -0.00033, 0.00110,
    0.00324, 0.00020, -0.00061,
    0.00040, 0.00374, 0.00016,
    -0.00022, -0.00273, -0.00016,
    0.00009, 0.00150, 0.00012,
    -0.00151, 0.00016, 0.00389,
    0.00103, -0.00007, -0.00283,
    -0.00050, 0.00000, 0.00156,
    -0.00600, -0.00022, 0.00103,
    0.00644, 0.00031, -0.00119,
    -0.00374, -0.00021, 0.00070,
    -0.00033, -0.00273, -0.00007,
    0.00031, 0.00287, 0.00013,
    -0.00014, -0.00170, -0.00012,
    0.00110, -0.00016, -0.00283,
    -0.00119, 0.00013, 0.00297,
    0.00063, -0.00004, -0.00177,
    0.00324, 0.00009, -0.00050,
    -0.00374, -0.00014, 0.00063,
    0.00495, 0.00024, -0.00093,
    0.00020, 0.00150, 0.00000,
    -0.00021, -0.00170, -0.00004,
    0.00024, 0.00214, 0.00012,
    -0.00061, 0.00012, 0.00156,
    0.00070, -0.00012, -0.00177,
    -0.00093, 0.00012, 0.00223
  ),
  nrow = 9
)

phi <- MCPhi(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  R = 1000L
)$output

# Specific time interval ----------------------------------------------------
PosteriorTotalCentral(
  phi = phi,
  delta_t = 1
)
#> Call:
#> PosteriorTotalCentral(phi = phi, delta_t = 1)
#> 
#> Total Effect Centrality
#>   variable interval    est     se    R    2.5%  97.5%
#> 1        x        1 0.3972 0.0482 1000  0.3026 0.4923
#> 2        m        1 0.4005 0.0417 1000  0.3140 0.4830
#> 3        y        1 0.0011 0.0659 1000 -0.1311 0.1271

# Range of time intervals ---------------------------------------------------
posterior <- PosteriorTotalCentral(
  phi = phi,
  delta_t = 1:5
)

# Methods -------------------------------------------------------------------
# PosteriorTotalCentral has a number of methods including
# print, summary, confint, and plot
print(posterior)
#> Call:
#> PosteriorTotalCentral(phi = phi, delta_t = 1:5)
#> 
#> Total Effect Centrality
#>    variable interval    est     se    R    2.5%  97.5%
#> 1         x        1 0.3972 0.0482 1000  0.3026 0.4923
#> 2         m        1 0.4005 0.0417 1000  0.3140 0.4830
#> 3         y        1 0.0011 0.0659 1000 -0.1311 0.1271
#> 4         x        2 0.7271 0.0680 1000  0.5992 0.8597
#> 5         m        2 0.4415 0.0528 1000  0.3364 0.5445
#> 6         y        2 0.0013 0.0970 1000 -0.1900 0.1815
#> 7         x        3 0.8861 0.0858 1000  0.7341 1.0611
#> 8         m        3 0.3671 0.0598 1000  0.2540 0.4857
#> 9         y        3 0.0009 0.1051 1000 -0.2120 0.1885
#> 10        x        4 0.9025 0.1009 1000  0.7206 1.1166
#> 11        m        4 0.2728 0.0648 1000  0.1533 0.4007
#> 12        y        4 0.0006 0.0998 1000 -0.2082 0.1831
#> 13        x        5 0.8309 0.1112 1000  0.6356 1.0648
#> 14        m        5 0.1915 0.0671 1000  0.0680 0.3324
#> 15        y        5 0.0008 0.0879 1000 -0.1831 0.1677
summary(posterior)
#> Call:
#> PosteriorTotalCentral(phi = phi, delta_t = 1:5)
#> 
#> Total Effect Centrality
#>    variable interval    est     se    R    2.5%  97.5%
#> 1         x        1 0.3972 0.0482 1000  0.3026 0.4923
#> 2         m        1 0.4005 0.0417 1000  0.3140 0.4830
#> 3         y        1 0.0011 0.0659 1000 -0.1311 0.1271
#> 4         x        2 0.7271 0.0680 1000  0.5992 0.8597
#> 5         m        2 0.4415 0.0528 1000  0.3364 0.5445
#> 6         y        2 0.0013 0.0970 1000 -0.1900 0.1815
#> 7         x        3 0.8861 0.0858 1000  0.7341 1.0611
#> 8         m        3 0.3671 0.0598 1000  0.2540 0.4857
#> 9         y        3 0.0009 0.1051 1000 -0.2120 0.1885
#> 10        x        4 0.9025 0.1009 1000  0.7206 1.1166
#> 11        m        4 0.2728 0.0648 1000  0.1533 0.4007
#> 12        y        4 0.0006 0.0998 1000 -0.2082 0.1831
#> 13        x        5 0.8309 0.1112 1000  0.6356 1.0648
#> 14        m        5 0.1915 0.0671 1000  0.0680 0.3324
#> 15        y        5 0.0008 0.0879 1000 -0.1831 0.1677
confint(posterior, level = 0.95)
#>    variable interval       2.5 %    97.5 %
#> 1         x        1  0.30260866 0.4922666
#> 2         m        1  0.31399992 0.4829705
#> 3         y        1 -0.13105883 0.1270621
#> 4         x        2  0.59915931 0.8596764
#> 5         m        2  0.33639670 0.5445221
#> 6         y        2 -0.19000702 0.1815099
#> 7         x        3  0.73413778 1.0610889
#> 8         m        3  0.25402960 0.4857251
#> 9         y        3 -0.21199863 0.1885317
#> 10        x        4  0.72057978 1.1165551
#> 11        m        4  0.15333527 0.4006763
#> 12        y        4 -0.20817860 0.1830511
#> 13        x        5  0.63556081 1.0647768
#> 14        m        5  0.06798658 0.3324359
#> 15        y        5 -0.18312024 0.1676966
plot(posterior)



```

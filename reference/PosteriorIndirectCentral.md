# Posterior Distribution of the Indirect Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function generates a posterior distribution of the indirect effect
centrality over a specific time interval \\\Delta t\\ or a range of time
intervals using the posterior distribution of the first-order stochastic
differential equation model drift matrix \\\boldsymbol{\Phi}\\.

## Usage

``` r
PosteriorIndirectCentral(phi, delta_t, ncores = NULL, tol = 0.01)
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

  Function used ("PosteriorIndirectCentral").

- output:

  A list the length of which is equal to the length of `delta_t`.

Each element in the `output` list has the following elements:

- est:

  Mean of the posterior distribution of the total, direct, and indirect
  effects.

- thetahatstar:

  Posterior distribution of the total, direct, and indirect effects.

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
[`BootIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/BootIndirectCentral.md),
[`BootMed()`](https://github.com/jeksterslab/cTMed/reference/BootMed.md),
[`BootMedStd()`](https://github.com/jeksterslab/cTMed/reference/BootMedStd.md),
[`BootTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/BootTotalCentral.md),
[`DeltaBeta()`](https://github.com/jeksterslab/cTMed/reference/DeltaBeta.md),
[`DeltaBetaStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaBetaStd.md),
[`DeltaDirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaDirectCentral.md),
[`DeltaIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaIndirectCentral.md),
[`DeltaMed()`](https://github.com/jeksterslab/cTMed/reference/DeltaMed.md),
[`DeltaMedStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaMedStd.md),
[`DeltaTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaTotalCentral.md),
[`Direct()`](https://github.com/jeksterslab/cTMed/reference/Direct.md),
[`DirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DirectCentral.md),
[`DirectStd()`](https://github.com/jeksterslab/cTMed/reference/DirectStd.md),
[`Indirect()`](https://github.com/jeksterslab/cTMed/reference/Indirect.md),
[`IndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/IndirectCentral.md),
[`IndirectStd()`](https://github.com/jeksterslab/cTMed/reference/IndirectStd.md),
[`MCBeta()`](https://github.com/jeksterslab/cTMed/reference/MCBeta.md),
[`MCBetaStd()`](https://github.com/jeksterslab/cTMed/reference/MCBetaStd.md),
[`MCDirectCentral()`](https://github.com/jeksterslab/cTMed/reference/MCDirectCentral.md),
[`MCIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/MCIndirectCentral.md),
[`MCMed()`](https://github.com/jeksterslab/cTMed/reference/MCMed.md),
[`MCMedStd()`](https://github.com/jeksterslab/cTMed/reference/MCMedStd.md),
[`MCPhi()`](https://github.com/jeksterslab/cTMed/reference/MCPhi.md),
[`MCPhiSigma()`](https://github.com/jeksterslab/cTMed/reference/MCPhiSigma.md),
[`MCTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/MCTotalCentral.md),
[`Med()`](https://github.com/jeksterslab/cTMed/reference/Med.md),
[`MedStd()`](https://github.com/jeksterslab/cTMed/reference/MedStd.md),
[`PosteriorBeta()`](https://github.com/jeksterslab/cTMed/reference/PosteriorBeta.md),
[`PosteriorDirectCentral()`](https://github.com/jeksterslab/cTMed/reference/PosteriorDirectCentral.md),
[`PosteriorMed()`](https://github.com/jeksterslab/cTMed/reference/PosteriorMed.md),
[`PosteriorTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/PosteriorTotalCentral.md),
[`Total()`](https://github.com/jeksterslab/cTMed/reference/Total.md),
[`TotalCentral()`](https://github.com/jeksterslab/cTMed/reference/TotalCentral.md),
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
PosteriorIndirectCentral(
  phi = phi,
  delta_t = 1
)
#> Call:
#> PosteriorIndirectCentral(phi = phi, delta_t = 1)
#> 
#> Indirect Effect Centrality
#>   variable interval    est     se    R    2.5%  97.5%
#> 1        x        1 0.0012 0.0198 1000 -0.0356 0.0386
#> 2        m        1 0.1660 0.0179 1000  0.1336 0.2015
#> 3        y        1 0.0012 0.0139 1000 -0.0256 0.0273

# Range of time intervals ---------------------------------------------------
posterior <- PosteriorIndirectCentral(
  phi = phi,
  delta_t = 1:5
)

# Methods -------------------------------------------------------------------
# PosteriorIndirectCentral has a number of methods including
# print, summary, confint, and plot
print(posterior)
#> Call:
#> PosteriorIndirectCentral(phi = phi, delta_t = 1:5)
#> 
#> Indirect Effect Centrality
#>    variable interval    est     se    R    2.5%  97.5%
#> 1         x        1 0.0012 0.0198 1000 -0.0356 0.0386
#> 2         m        1 0.1660 0.0179 1000  0.1336 0.2015
#> 3         y        1 0.0012 0.0139 1000 -0.0256 0.0273
#> 4         x        2 0.0021 0.0388 1000 -0.0724 0.0724
#> 5         m        2 0.3991 0.0476 1000  0.3140 0.4955
#> 6         y        2 0.0026 0.0320 1000 -0.0565 0.0631
#> 7         x        3 0.0016 0.0471 1000 -0.0909 0.0904
#> 8         m        3 0.5429 0.0737 1000  0.4182 0.7040
#> 9         y        3 0.0028 0.0480 1000 -0.0858 0.0910
#> 10        x        4 0.0007 0.0518 1000 -0.1032 0.1007
#> 11        m        4 0.5868 0.0903 1000  0.4383 0.7983
#> 12        y        4 0.0020 0.0638 1000 -0.1206 0.1184
#> 13        x        5 0.0001 0.0556 1000 -0.1142 0.1062
#> 14        m        5 0.5607 0.0971 1000  0.4051 0.7926
#> 15        y        5 0.0009 0.0787 1000 -0.1532 0.1472
summary(posterior)
#> Call:
#> PosteriorIndirectCentral(phi = phi, delta_t = 1:5)
#> 
#> Indirect Effect Centrality
#>    variable interval    est     se    R    2.5%  97.5%
#> 1         x        1 0.0012 0.0198 1000 -0.0356 0.0386
#> 2         m        1 0.1660 0.0179 1000  0.1336 0.2015
#> 3         y        1 0.0012 0.0139 1000 -0.0256 0.0273
#> 4         x        2 0.0021 0.0388 1000 -0.0724 0.0724
#> 5         m        2 0.3991 0.0476 1000  0.3140 0.4955
#> 6         y        2 0.0026 0.0320 1000 -0.0565 0.0631
#> 7         x        3 0.0016 0.0471 1000 -0.0909 0.0904
#> 8         m        3 0.5429 0.0737 1000  0.4182 0.7040
#> 9         y        3 0.0028 0.0480 1000 -0.0858 0.0910
#> 10        x        4 0.0007 0.0518 1000 -0.1032 0.1007
#> 11        m        4 0.5868 0.0903 1000  0.4383 0.7983
#> 12        y        4 0.0020 0.0638 1000 -0.1206 0.1184
#> 13        x        5 0.0001 0.0556 1000 -0.1142 0.1062
#> 14        m        5 0.5607 0.0971 1000  0.4051 0.7926
#> 15        y        5 0.0009 0.0787 1000 -0.1532 0.1472
confint(posterior, level = 0.95)
#>    variable interval       2.5 %     97.5 %
#> 1         x        1 -0.03562903 0.03858116
#> 2         m        1  0.13355604 0.20146626
#> 3         y        1 -0.02560192 0.02732945
#> 4         x        2 -0.07244090 0.07244703
#> 5         m        2  0.31396819 0.49552641
#> 6         y        2 -0.05645701 0.06313106
#> 7         x        3 -0.09088982 0.09039606
#> 8         m        3  0.41818122 0.70401691
#> 9         y        3 -0.08580920 0.09103733
#> 10        x        4 -0.10316810 0.10071590
#> 11        m        4  0.43826382 0.79833319
#> 12        y        4 -0.12059238 0.11841166
#> 13        x        5 -0.11416462 0.10621817
#> 14        m        5  0.40510439 0.79260397
#> 15        y        5 -0.15315646 0.14723179
plot(posterior)



```

# Posterior Distribution of Total, Direct, and Indirect Effects of X on Y Through M Over a Specific Time Interval or a Range of Time Intervals

This function generates a posterior distribution of the total, direct
and indirect effects of the independent variable \\X\\ on the dependent
variable \\Y\\ through mediator variables \\\mathbf{m}\\ over a specific
time interval \\\Delta t\\ or a range of time intervals using the
posterior distribution of the first-order stochastic differential
equation model drift matrix \\\boldsymbol{\Phi}\\.

## Usage

``` r
PosteriorMed(phi, delta_t, from, to, med, ncores = NULL, tol = 0.01)
```

## Arguments

- phi:

  List of numeric matrices. Each element of the list is a sample from
  the posterior distribution of the drift matrix
  (\\\boldsymbol{\Phi}\\). Each matrix should have row and column names
  pertaining to the variables in the system.

- delta_t:

  Numeric. Time interval (\\\Delta t\\).

- from:

  Character string. Name of the independent variable \\X\\ in `phi`.

- to:

  Character string. Name of the dependent variable \\Y\\ in `phi`.

- med:

  Character vector. Name/s of the mediator variable/s in `phi`.

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

  Function used ("PosteriorMed").

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
[`Total()`](https://github.com/jeksterslab/cTMed/reference/Total.md),
[`Direct()`](https://github.com/jeksterslab/cTMed/reference/Direct.md),
and
[`Indirect()`](https://github.com/jeksterslab/cTMed/reference/Indirect.md)
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
[`PosteriorIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/PosteriorIndirectCentral.md),
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
PosteriorMed(
  phi = phi,
  delta_t = 1,
  from = "x",
  to = "y",
  med = "m"
)
#> Call:
#> PosteriorMed(phi = phi, delta_t = 1, from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>     effect interval     est     se    R    2.5%   97.5%
#> 1    total        1 -0.0998 0.0300 1000 -0.1541 -0.0428
#> 2   direct        1 -0.2681 0.0394 1000 -0.3443 -0.1939
#> 3 indirect        1  0.1683 0.0178 1000  0.1340  0.2037

# Range of time intervals ---------------------------------------------------
posterior <- PosteriorMed(
  phi = phi,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m"
)

# Methods -------------------------------------------------------------------
# PosteriorMed has a number of methods including
# print, summary, confint, and plot
print(posterior)
#> Call:
#> PosteriorMed(phi = phi, delta_t = 1:5, from = "x", to = "y", 
#>     med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      effect interval     est     se    R    2.5%   97.5%
#> 1     total        1 -0.0998 0.0300 1000 -0.1541 -0.0428
#> 2    direct        1 -0.2681 0.0394 1000 -0.3443 -0.1939
#> 3  indirect        1  0.1683 0.0178 1000  0.1340  0.2037
#> 4     total        2  0.0802 0.0329 1000  0.0145  0.1446
#> 5    direct        2 -0.3231 0.0560 1000 -0.4327 -0.2247
#> 6  indirect        2  0.4033 0.0472 1000  0.3157  0.5009
#> 7     total        3  0.2515 0.0342 1000  0.1849  0.3216
#> 8    direct        3 -0.2957 0.0626 1000 -0.4238 -0.1898
#> 9  indirect        3  0.5471 0.0728 1000  0.4195  0.7048
#> 10    total        4  0.3465 0.0393 1000  0.2786  0.4346
#> 11   direct        4 -0.2436 0.0633 1000 -0.3827 -0.1413
#> 12 indirect        4  0.5901 0.0889 1000  0.4436  0.7843
#> 13    total        5  0.3722 0.0446 1000  0.2983  0.4719
#> 14   direct        5 -0.1906 0.0603 1000 -0.3309 -0.1006
#> 15 indirect        5  0.5628 0.0951 1000  0.4087  0.7885
summary(posterior)
#> Call:
#> PosteriorMed(phi = phi, delta_t = 1:5, from = "x", to = "y", 
#>     med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      effect interval     est     se    R    2.5%   97.5%
#> 1     total        1 -0.0998 0.0300 1000 -0.1541 -0.0428
#> 2    direct        1 -0.2681 0.0394 1000 -0.3443 -0.1939
#> 3  indirect        1  0.1683 0.0178 1000  0.1340  0.2037
#> 4     total        2  0.0802 0.0329 1000  0.0145  0.1446
#> 5    direct        2 -0.3231 0.0560 1000 -0.4327 -0.2247
#> 6  indirect        2  0.4033 0.0472 1000  0.3157  0.5009
#> 7     total        3  0.2515 0.0342 1000  0.1849  0.3216
#> 8    direct        3 -0.2957 0.0626 1000 -0.4238 -0.1898
#> 9  indirect        3  0.5471 0.0728 1000  0.4195  0.7048
#> 10    total        4  0.3465 0.0393 1000  0.2786  0.4346
#> 11   direct        4 -0.2436 0.0633 1000 -0.3827 -0.1413
#> 12 indirect        4  0.5901 0.0889 1000  0.4436  0.7843
#> 13    total        5  0.3722 0.0446 1000  0.2983  0.4719
#> 14   direct        5 -0.1906 0.0603 1000 -0.3309 -0.1006
#> 15 indirect        5  0.5628 0.0951 1000  0.4087  0.7885
confint(posterior, level = 0.95)
#>      effect interval      2.5 %      97.5 %
#> 1     total        1 -0.1541331 -0.04278507
#> 2    direct        1 -0.3442722 -0.19389771
#> 3  indirect        1  0.1339817  0.20369213
#> 4     total        2  0.0145340  0.14459580
#> 5    direct        2 -0.4326701 -0.22470431
#> 6  indirect        2  0.3157411  0.50092997
#> 7     total        3  0.1849007  0.32164073
#> 8    direct        3 -0.4237842 -0.18982649
#> 9  indirect        3  0.4194591  0.70481461
#> 10    total        4  0.2786364  0.43455167
#> 11   direct        4 -0.3827311 -0.14134158
#> 12 indirect        4  0.4436055  0.78428773
#> 13    total        5  0.2983093  0.47189128
#> 14   direct        5 -0.3308675 -0.10056713
#> 15 indirect        5  0.4086515  0.78851620
plot(posterior)



```

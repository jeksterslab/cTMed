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
[`BootIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/BootIndirectCentral.md),
[`BootMed()`](https://github.com/jeksterslab/cTMed/reference/BootMed.md),
[`BootMedStd()`](https://github.com/jeksterslab/cTMed/reference/BootMedStd.md),
[`BootTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/BootTotalCentral.md),
[`DeltaBeta()`](https://github.com/jeksterslab/cTMed/reference/DeltaBeta.md),
[`DeltaBetaStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaBetaStd.md),
[`DeltaIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaIndirectCentral.md),
[`DeltaMed()`](https://github.com/jeksterslab/cTMed/reference/DeltaMed.md),
[`DeltaMedStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaMedStd.md),
[`DeltaTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaTotalCentral.md),
[`Direct()`](https://github.com/jeksterslab/cTMed/reference/Direct.md),
[`DirectStd()`](https://github.com/jeksterslab/cTMed/reference/DirectStd.md),
[`Indirect()`](https://github.com/jeksterslab/cTMed/reference/Indirect.md),
[`IndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/IndirectCentral.md),
[`IndirectStd()`](https://github.com/jeksterslab/cTMed/reference/IndirectStd.md),
[`MCBeta()`](https://github.com/jeksterslab/cTMed/reference/MCBeta.md),
[`MCBetaStd()`](https://github.com/jeksterslab/cTMed/reference/MCBetaStd.md),
[`MCIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/MCIndirectCentral.md),
[`MCMed()`](https://github.com/jeksterslab/cTMed/reference/MCMed.md),
[`MCMedStd()`](https://github.com/jeksterslab/cTMed/reference/MCMedStd.md),
[`MCPhi()`](https://github.com/jeksterslab/cTMed/reference/MCPhi.md),
[`MCPhiSigma()`](https://github.com/jeksterslab/cTMed/reference/MCPhiSigma.md),
[`MCTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/MCTotalCentral.md),
[`Med()`](https://github.com/jeksterslab/cTMed/reference/Med.md),
[`MedStd()`](https://github.com/jeksterslab/cTMed/reference/MedStd.md),
[`PosteriorBeta()`](https://github.com/jeksterslab/cTMed/reference/PosteriorBeta.md),
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
#> 1    total        1 -0.0998 0.0307 1000 -0.1665 -0.0426
#> 2   direct        1 -0.2664 0.0395 1000 -0.3488 -0.1949
#> 3 indirect        1  0.1666 0.0175 1000  0.1345  0.2033

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
#> 1     total        1 -0.0998 0.0307 1000 -0.1665 -0.0426
#> 2    direct        1 -0.2664 0.0395 1000 -0.3488 -0.1949
#> 3  indirect        1  0.1666 0.0175 1000  0.1345  0.2033
#> 4     total        2  0.0791 0.0343 1000  0.0044  0.1380
#> 5    direct        2 -0.3209 0.0564 1000 -0.4448 -0.2203
#> 6  indirect        2  0.4000 0.0465 1000  0.3159  0.4971
#> 7     total        3  0.2497 0.0350 1000  0.1816  0.3176
#> 8    direct        3 -0.2937 0.0637 1000 -0.4384 -0.1875
#> 9  indirect        3  0.5433 0.0723 1000  0.4154  0.6986
#> 10    total        4  0.3442 0.0393 1000  0.2728  0.4219
#> 11   direct        4 -0.2419 0.0649 1000 -0.3952 -0.1405
#> 12 indirect        4  0.5862 0.0890 1000  0.4338  0.7824
#> 13    total        5  0.3695 0.0444 1000  0.2879  0.4614
#> 14   direct        5 -0.1893 0.0622 1000 -0.3465 -0.0973
#> 15 indirect        5  0.5588 0.0958 1000  0.4051  0.7721
summary(posterior)
#> Call:
#> PosteriorMed(phi = phi, delta_t = 1:5, from = "x", to = "y", 
#>     med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      effect interval     est     se    R    2.5%   97.5%
#> 1     total        1 -0.0998 0.0307 1000 -0.1665 -0.0426
#> 2    direct        1 -0.2664 0.0395 1000 -0.3488 -0.1949
#> 3  indirect        1  0.1666 0.0175 1000  0.1345  0.2033
#> 4     total        2  0.0791 0.0343 1000  0.0044  0.1380
#> 5    direct        2 -0.3209 0.0564 1000 -0.4448 -0.2203
#> 6  indirect        2  0.4000 0.0465 1000  0.3159  0.4971
#> 7     total        3  0.2497 0.0350 1000  0.1816  0.3176
#> 8    direct        3 -0.2937 0.0637 1000 -0.4384 -0.1875
#> 9  indirect        3  0.5433 0.0723 1000  0.4154  0.6986
#> 10    total        4  0.3442 0.0393 1000  0.2728  0.4219
#> 11   direct        4 -0.2419 0.0649 1000 -0.3952 -0.1405
#> 12 indirect        4  0.5862 0.0890 1000  0.4338  0.7824
#> 13    total        5  0.3695 0.0444 1000  0.2879  0.4614
#> 14   direct        5 -0.1893 0.0622 1000 -0.3465 -0.0973
#> 15 indirect        5  0.5588 0.0958 1000  0.4051  0.7721
confint(posterior, level = 0.95)
#>      effect interval        2.5 %      97.5 %
#> 1     total        1 -0.166470645 -0.04260420
#> 2    direct        1 -0.348803573 -0.19489083
#> 3  indirect        1  0.134503056  0.20333979
#> 4     total        2  0.004369168  0.13802573
#> 5    direct        2 -0.444768981 -0.22030376
#> 6  indirect        2  0.315851481  0.49710561
#> 7     total        3  0.181639312  0.31755141
#> 8    direct        3 -0.438396767 -0.18748070
#> 9  indirect        3  0.415430083  0.69859417
#> 10    total        4  0.272760335  0.42191848
#> 11   direct        4 -0.395163282 -0.14053097
#> 12 indirect        4  0.433771997  0.78244742
#> 13    total        5  0.287872340  0.46141766
#> 14   direct        5 -0.346457942 -0.09726921
#> 15 indirect        5  0.405107599  0.77212881
plot(posterior)



```

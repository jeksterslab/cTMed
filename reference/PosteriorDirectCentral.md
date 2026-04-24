# Posterior Distribution of the Direct Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function generates a posterior distribution of the direct effect
centrality over a specific time interval \\\Delta t\\ or a range of time
intervals using the posterior distribution of the first-order stochastic
differential equation model drift matrix \\\boldsymbol{\Phi}\\.

## Usage

``` r
PosteriorDirectCentral(phi, delta_t, ncores = NULL, tol = 0.01)
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

  Function used ("PosteriorDirectCentral").

- output:

  A list the length of which is equal to the length of `delta_t`.

Each element in the `output` list has the following elements:

- est:

  Mean of the posterior distribution of the total, direct, and direct
  effects.

- thetahatstar:

  Posterior distribution of the total, direct, and direct effects.

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
[`PosteriorIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/PosteriorIndirectCentral.md),
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
PosteriorDirectCentral(
  phi = phi,
  delta_t = 1
)
#> Call:
#> PosteriorDirectCentral(phi = phi, delta_t = 1)
#> 
#> Direct Effect Centrality
#>   variable interval     est     se    R    2.5%   97.5%
#> 1        x        1  0.3994 0.0329 1000  0.3389  0.4628
#> 2        m        1 -0.2669 0.0507 1000 -0.3641 -0.1712
#> 3        y        1  0.4973 0.0503 1000  0.3932  0.5886

# Range of time intervals ---------------------------------------------------
posterior <- PosteriorDirectCentral(
  phi = phi,
  delta_t = 1:5
)

# Methods -------------------------------------------------------------------
# PosteriorDirectCentral has a number of methods including
# print, summary, confint, and plot
print(posterior)
#> Call:
#> PosteriorDirectCentral(phi = phi, delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se    R    2.5%   97.5%
#> 1         x        1  0.3994 0.0329 1000  0.3389  0.4628
#> 2         m        1 -0.2669 0.0507 1000 -0.3641 -0.1712
#> 3         y        1  0.4973 0.0503 1000  0.3932  0.5886
#> 4         x        2  0.4396 0.0371 1000  0.3714  0.5124
#> 5         m        2 -0.3217 0.0659 1000 -0.4561 -0.2040
#> 6         y        2  0.6470 0.0656 1000  0.5213  0.7728
#> 7         x        3  0.3645 0.0369 1000  0.2953  0.4350
#> 8         m        3 -0.2949 0.0698 1000 -0.4433 -0.1709
#> 9         y        3  0.6340 0.0801 1000  0.4882  0.8026
#> 10        x        4  0.2698 0.0347 1000  0.2054  0.3402
#> 11        m        4 -0.2435 0.0686 1000 -0.3926 -0.1277
#> 12        y        4  0.5544 0.0937 1000  0.3834  0.7538
#> 13        x        5  0.1880 0.0308 1000  0.1324  0.2525
#> 14        m        5 -0.1910 0.0643 1000 -0.3336 -0.0912
#> 15        y        5  0.4563 0.1033 1000  0.2743  0.6748
summary(posterior)
#> Call:
#> PosteriorDirectCentral(phi = phi, delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se    R    2.5%   97.5%
#> 1         x        1  0.3994 0.0329 1000  0.3389  0.4628
#> 2         m        1 -0.2669 0.0507 1000 -0.3641 -0.1712
#> 3         y        1  0.4973 0.0503 1000  0.3932  0.5886
#> 4         x        2  0.4396 0.0371 1000  0.3714  0.5124
#> 5         m        2 -0.3217 0.0659 1000 -0.4561 -0.2040
#> 6         y        2  0.6470 0.0656 1000  0.5213  0.7728
#> 7         x        3  0.3645 0.0369 1000  0.2953  0.4350
#> 8         m        3 -0.2949 0.0698 1000 -0.4433 -0.1709
#> 9         y        3  0.6340 0.0801 1000  0.4882  0.8026
#> 10        x        4  0.2698 0.0347 1000  0.2054  0.3402
#> 11        m        4 -0.2435 0.0686 1000 -0.3926 -0.1277
#> 12        y        4  0.5544 0.0937 1000  0.3834  0.7538
#> 13        x        5  0.1880 0.0308 1000  0.1324  0.2525
#> 14        m        5 -0.1910 0.0643 1000 -0.3336 -0.0912
#> 15        y        5  0.4563 0.1033 1000  0.2743  0.6748
confint(posterior, level = 0.95)
#>    variable interval      2.5 %     97.5 %
#> 1         x        1  0.3388752  0.4628484
#> 2         m        1 -0.3640951 -0.1711676
#> 3         y        1  0.3932128  0.5885957
#> 4         x        2  0.3714438  0.5123605
#> 5         m        2 -0.4560577 -0.2040325
#> 6         y        2  0.5213004  0.7727990
#> 7         x        3  0.2953211  0.4350341
#> 8         m        3 -0.4433119 -0.1709108
#> 9         y        3  0.4882250  0.8025748
#> 10        x        4  0.2054495  0.3401825
#> 11        m        4 -0.3925611 -0.1277065
#> 12        y        4  0.3834199  0.7537579
#> 13        x        5  0.1323606  0.2525112
#> 14        m        5 -0.3335932 -0.0912327
#> 15        y        5  0.2742829  0.6747571
plot(posterior)



```

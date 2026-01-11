# Posterior Sampling Distribution for the Elements of the Matrix of Lagged Coefficients Over a Specific Time Interval or a Range of Time Intervals

This function generates a posterior sampling distribution for the
elements of the matrix of lagged coefficients \\\boldsymbol{\beta}\\
over a specific time interval \\\Delta t\\ or a range of time intervals
using the first-order stochastic differential equation model drift
matrix \\\boldsymbol{\Phi}\\.

## Usage

``` r
PosteriorBeta(phi, delta_t, ncores = NULL, tol = 0.01)
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

  Function used ("PosteriorBeta").

- output:

  A list the length of which is equal to the length of `delta_t`.

Each element in the `output` list has the following elements:

- est:

  A vector of total, direct, and indirect effects.

- thetahatstar:

  A matrix of Monte Carlo total, direct, and indirect effects.

## Details

See
[`Total()`](https://github.com/jeksterslab/cTMed/reference/Total.md).

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
PosteriorBeta(
  phi = phi,
  delta_t = 1
)
#> Call:
#> PosteriorBeta(phi = phi, delta_t = 1)
#> 
#> Total, Direct, and Indirect Effects
#> 
#>        effect interval     est     se    R    2.5%   97.5%
#> 1 from x to x        1  0.7007 0.0488 1000  0.6088  0.8038
#> 2 from x to m        1  0.5022 0.0351 1000  0.4361  0.5713
#> 3 from x to y        1 -0.1011 0.0318 1000 -0.1678 -0.0433
#> 4 from m to x        1 -0.0001 0.0440 1000 -0.0901  0.0794
#> 5 from m to m        1  0.5999 0.0332 1000  0.5334  0.6655
#> 6 from m to y        1  0.4008 0.0282 1000  0.3472  0.4588
#> 7 from y to x        1  0.0001 0.0402 1000 -0.0801  0.0798
#> 8 from y to m        1  0.0007 0.0319 1000 -0.0637  0.0631
#> 9 from y to y        1  0.4995 0.0277 1000  0.4446  0.5535

# Range of time intervals ---------------------------------------------------
posterior <- PosteriorBeta(
  phi = phi,
  delta_t = 1:5
)
plot(posterior)










# Methods -------------------------------------------------------------------
# PosteriorBeta has a number of methods including
# print, summary, confint, and plot
print(posterior)
#> Call:
#> PosteriorBeta(phi = phi, delta_t = 1:5)
#> 
#> Total, Direct, and Indirect Effects
#> 
#>         effect interval     est     se    R    2.5%   97.5%
#> 1  from x to x        1  0.7007 0.0488 1000  0.6088  0.8038
#> 2  from x to m        1  0.5022 0.0351 1000  0.4361  0.5713
#> 3  from x to y        1 -0.1011 0.0318 1000 -0.1678 -0.0433
#> 4  from m to x        1 -0.0001 0.0440 1000 -0.0901  0.0794
#> 5  from m to m        1  0.5999 0.0332 1000  0.5334  0.6655
#> 6  from m to y        1  0.4008 0.0282 1000  0.3472  0.4588
#> 7  from y to x        1  0.0001 0.0402 1000 -0.0801  0.0798
#> 8  from y to m        1  0.0007 0.0319 1000 -0.0637  0.0631
#> 9  from y to y        1  0.4995 0.0277 1000  0.4446  0.5535
#> 10 from x to x        2  0.4909 0.0572 1000  0.3899  0.6175
#> 11 from x to m        2  0.6530 0.0545 1000  0.5520  0.7660
#> 12 from x to y        2  0.0799 0.0348 1000  0.0074  0.1436
#> 13 from m to x        2 -0.0001 0.0531 1000 -0.1129  0.0945
#> 14 from m to m        2  0.3601 0.0517 1000  0.2555  0.4611
#> 15 from m to y        2  0.4407 0.0325 1000  0.3817  0.5102
#> 16 from y to x        2  0.0001 0.0484 1000 -0.0963  0.0965
#> 17 from y to m        2  0.0008 0.0494 1000 -0.1009  0.0992
#> 18 from y to y        2  0.2498 0.0327 1000  0.1846  0.3119
#> 19 from x to x        3  0.3439 0.0568 1000  0.2462  0.4674
#> 20 from x to m        3  0.6383 0.0673 1000  0.5159  0.7808
#> 21 from x to y        3  0.2521 0.0343 1000  0.1835  0.3203
#> 22 from m to x        3 -0.0001 0.0521 1000 -0.1111  0.0953
#> 23 from m to m        3  0.2162 0.0635 1000  0.0864  0.3347
#> 24 from m to y        3  0.3645 0.0329 1000  0.3076  0.4421
#> 25 from y to x        3  0.0001 0.0443 1000 -0.0899  0.0867
#> 26 from y to m        3  0.0007 0.0581 1000 -0.1192  0.1182
#> 27 from y to y        3  0.1251 0.0311 1000  0.0667  0.1868
#> 28 from x to x        4  0.2409 0.0554 1000  0.1459  0.3639
#> 29 from x to m        4  0.5558 0.0741 1000  0.4306  0.7278
#> 30 from x to y        4  0.3470 0.0383 1000  0.2738  0.4232
#> 31 from m to x        4 -0.0001 0.0479 1000 -0.0995  0.0879
#> 32 from m to m        4  0.1299 0.0686 1000 -0.0070  0.2599
#> 33 from m to y        4  0.2688 0.0361 1000  0.2035  0.3518
#> 34 from y to x        4  0.0000 0.0365 1000 -0.0735  0.0694
#> 35 from y to m        4  0.0005 0.0591 1000 -0.1193  0.1187
#> 36 from y to y        4  0.0628 0.0318 1000  0.0034  0.1238
#> 37 from x to x        5  0.1688 0.0546 1000  0.0729  0.2879
#> 38 from x to m        5  0.4546 0.0774 1000  0.3254  0.6292
#> 39 from x to y        5  0.3718 0.0438 1000  0.2943  0.4644
#> 40 from m to x        5 -0.0001 0.0422 1000 -0.0815  0.0809
#> 41 from m to m        5  0.0781 0.0685 1000 -0.0525  0.2114
#> 42 from m to y        5  0.1863 0.0400 1000  0.1114  0.2737
#> 43 from y to x        5  0.0000 0.0286 1000 -0.0569  0.0575
#> 44 from y to m        5  0.0004 0.0549 1000 -0.1088  0.1073
#> 45 from y to y        5  0.0316 0.0342 1000 -0.0346  0.1011
summary(posterior)
#> Call:
#> PosteriorBeta(phi = phi, delta_t = 1:5)
#> 
#> Total, Direct, and Indirect Effects
#> 
#>         effect interval     est     se    R    2.5%   97.5%
#> 1  from x to x        1  0.7007 0.0488 1000  0.6088  0.8038
#> 2  from x to m        1  0.5022 0.0351 1000  0.4361  0.5713
#> 3  from x to y        1 -0.1011 0.0318 1000 -0.1678 -0.0433
#> 4  from m to x        1 -0.0001 0.0440 1000 -0.0901  0.0794
#> 5  from m to m        1  0.5999 0.0332 1000  0.5334  0.6655
#> 6  from m to y        1  0.4008 0.0282 1000  0.3472  0.4588
#> 7  from y to x        1  0.0001 0.0402 1000 -0.0801  0.0798
#> 8  from y to m        1  0.0007 0.0319 1000 -0.0637  0.0631
#> 9  from y to y        1  0.4995 0.0277 1000  0.4446  0.5535
#> 10 from x to x        2  0.4909 0.0572 1000  0.3899  0.6175
#> 11 from x to m        2  0.6530 0.0545 1000  0.5520  0.7660
#> 12 from x to y        2  0.0799 0.0348 1000  0.0074  0.1436
#> 13 from m to x        2 -0.0001 0.0531 1000 -0.1129  0.0945
#> 14 from m to m        2  0.3601 0.0517 1000  0.2555  0.4611
#> 15 from m to y        2  0.4407 0.0325 1000  0.3817  0.5102
#> 16 from y to x        2  0.0001 0.0484 1000 -0.0963  0.0965
#> 17 from y to m        2  0.0008 0.0494 1000 -0.1009  0.0992
#> 18 from y to y        2  0.2498 0.0327 1000  0.1846  0.3119
#> 19 from x to x        3  0.3439 0.0568 1000  0.2462  0.4674
#> 20 from x to m        3  0.6383 0.0673 1000  0.5159  0.7808
#> 21 from x to y        3  0.2521 0.0343 1000  0.1835  0.3203
#> 22 from m to x        3 -0.0001 0.0521 1000 -0.1111  0.0953
#> 23 from m to m        3  0.2162 0.0635 1000  0.0864  0.3347
#> 24 from m to y        3  0.3645 0.0329 1000  0.3076  0.4421
#> 25 from y to x        3  0.0001 0.0443 1000 -0.0899  0.0867
#> 26 from y to m        3  0.0007 0.0581 1000 -0.1192  0.1182
#> 27 from y to y        3  0.1251 0.0311 1000  0.0667  0.1868
#> 28 from x to x        4  0.2409 0.0554 1000  0.1459  0.3639
#> 29 from x to m        4  0.5558 0.0741 1000  0.4306  0.7278
#> 30 from x to y        4  0.3470 0.0383 1000  0.2738  0.4232
#> 31 from m to x        4 -0.0001 0.0479 1000 -0.0995  0.0879
#> 32 from m to m        4  0.1299 0.0686 1000 -0.0070  0.2599
#> 33 from m to y        4  0.2688 0.0361 1000  0.2035  0.3518
#> 34 from y to x        4  0.0000 0.0365 1000 -0.0735  0.0694
#> 35 from y to m        4  0.0005 0.0591 1000 -0.1193  0.1187
#> 36 from y to y        4  0.0628 0.0318 1000  0.0034  0.1238
#> 37 from x to x        5  0.1688 0.0546 1000  0.0729  0.2879
#> 38 from x to m        5  0.4546 0.0774 1000  0.3254  0.6292
#> 39 from x to y        5  0.3718 0.0438 1000  0.2943  0.4644
#> 40 from m to x        5 -0.0001 0.0422 1000 -0.0815  0.0809
#> 41 from m to m        5  0.0781 0.0685 1000 -0.0525  0.2114
#> 42 from m to y        5  0.1863 0.0400 1000  0.1114  0.2737
#> 43 from y to x        5  0.0000 0.0286 1000 -0.0569  0.0575
#> 44 from y to m        5  0.0004 0.0549 1000 -0.1088  0.1073
#> 45 from y to y        5  0.0316 0.0342 1000 -0.0346  0.1011
confint(posterior, level = 0.95)
#>         effect interval        2.5 %      97.5 %
#> 1  from x to x        1  0.608787051  0.80378813
#> 2  from x to m        1  0.436064479  0.57132115
#> 3  from x to y        1 -0.167796773 -0.04326155
#> 4  from x to x        2  0.389924244  0.61745605
#> 5  from x to m        2  0.552019346  0.76595378
#> 6  from x to y        2  0.007428498  0.14362689
#> 7  from x to x        3  0.246244665  0.46744703
#> 8  from x to m        3  0.515931515  0.78083560
#> 9  from x to y        3  0.183482045  0.32030654
#> 10 from x to x        4  0.145868037  0.36392793
#> 11 from x to m        4  0.430579036  0.72776025
#> 12 from x to y        4  0.273844483  0.42324830
#> 13 from x to x        5  0.072856859  0.28794674
#> 14 from x to m        5  0.325389617  0.62922260
#> 15 from x to y        5  0.294271102  0.46444840
plot(posterior)









```

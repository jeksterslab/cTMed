# Delta Method Sampling Variance-Covariance Matrix for the Standardized Total Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function computes the delta method sampling variance-covariance
matrix for the standardized total effect centrality over a specific time
interval \\\Delta t\\ or a range of time intervals using the first-order
stochastic differential equation model's drift matrix
\\\boldsymbol{\Phi}\\ and process noise covariance matrix
\\\boldsymbol{\Sigma}\\.

## Usage

``` r
DeltaTotalCentralStd(
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

  Function used ("DeltaTotalCentralStd").

- output:

  A list of length `length(delta_t)`.

Each element in the `output` list has the following elements:

- delta_t:

  Time interval.

- jacobian:

  Jacobian matrix.

- est:

  Estimated standardized total effect centrality.

- vcov:

  Sampling variance-covariance matrix of estimated standardized total
  effect centrality.

## Details

See
[`TotalCentralStd()`](https://github.com/jeksterslab/cTMed/reference/TotalCentralStd.md)
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
[`DeltaDirectCentralStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaDirectCentralStd.md),
[`DeltaIndirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaIndirectCentral.md),
[`DeltaMed()`](https://github.com/jeksterslab/cTMed/reference/DeltaMed.md),
[`DeltaMedStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaMedStd.md),
[`DeltaTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaTotalCentral.md),
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
DeltaTotalCentralStd(
  phi = phi,
  sigma = sigma,
  vcov_theta = vcov_theta,
  delta_t = 1
)
#> Call:
#> DeltaTotalCentralStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1)
#> 
#> Total Effect Centrality
#>   variable interval    est     se      z p    2.5%  97.5%
#> 1        x        1 0.2819 0.0438 6.4424 0  0.1962 0.3677
#> 2        m        1 0.5494 0.0599 9.1793 0  0.4321 0.6667
#> 3        y        1 0.0000 0.0554 0.0000 1 -0.1086 0.1086

# Range of time intervals ---------------------------------------------------
delta <- DeltaTotalCentralStd(
  phi = phi,
  sigma = sigma,
  vcov_theta = vcov_theta,
  delta_t = 1:5
)
plot(delta)




# Methods -------------------------------------------------------------------
# DeltaTotalCentralStd has a number of methods including
# print, summary, confint, and plot
print(delta)
#> Call:
#> DeltaTotalCentralStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1:5)
#> 
#> Total Effect Centrality
#>    variable interval    est     se       z      p    2.5%  97.5%
#> 1         x        1 0.2819 0.0438  6.4424 0.0000  0.1962 0.3677
#> 2         m        1 0.5494 0.0599  9.1793 0.0000  0.4321 0.6667
#> 3         y        1 0.0000 0.0554  0.0000 1.0000 -0.1086 0.1086
#> 4         x        2 0.5907 0.0543 10.8715 0.0000  0.4842 0.6972
#> 5         m        2 0.6044 0.0744  8.1245 0.0000  0.4586 0.7502
#> 6         y        2 0.0000 0.0792  0.0000 1.0000 -0.1553 0.1553
#> 7         x        3 0.7616 0.0638 11.9328 0.0000  0.6365 0.8866
#> 8         m        3 0.4999 0.0833  5.9990 0.0000  0.3366 0.6633
#> 9         y        3 0.0000 0.0837  0.0000 1.0000 -0.1640 0.1640
#> 10        x        4 0.7979 0.0738 10.8167 0.0000  0.6533 0.9425
#> 11        m        4 0.3686 0.0899  4.1016 0.0000  0.1925 0.5448
#> 12        y        4 0.0000 0.0775  0.0000 1.0000 -0.1520 0.1520
#> 13        x        5 0.7453 0.0823  9.0536 0.0000  0.5840 0.9067
#> 14        m        5 0.2555 0.0925  2.7625 0.0057  0.0742 0.4368
#> 15        y        5 0.0000 0.0667  0.0000 1.0000 -0.1308 0.1308
summary(delta)
#> Call:
#> DeltaTotalCentralStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1:5)
#> 
#> Total Effect Centrality
#>    variable interval    est     se       z      p    2.5%  97.5%
#> 1         x        1 0.2819 0.0438  6.4424 0.0000  0.1962 0.3677
#> 2         m        1 0.5494 0.0599  9.1793 0.0000  0.4321 0.6667
#> 3         y        1 0.0000 0.0554  0.0000 1.0000 -0.1086 0.1086
#> 4         x        2 0.5907 0.0543 10.8715 0.0000  0.4842 0.6972
#> 5         m        2 0.6044 0.0744  8.1245 0.0000  0.4586 0.7502
#> 6         y        2 0.0000 0.0792  0.0000 1.0000 -0.1553 0.1553
#> 7         x        3 0.7616 0.0638 11.9328 0.0000  0.6365 0.8866
#> 8         m        3 0.4999 0.0833  5.9990 0.0000  0.3366 0.6633
#> 9         y        3 0.0000 0.0837  0.0000 1.0000 -0.1640 0.1640
#> 10        x        4 0.7979 0.0738 10.8167 0.0000  0.6533 0.9425
#> 11        m        4 0.3686 0.0899  4.1016 0.0000  0.1925 0.5448
#> 12        y        4 0.0000 0.0775  0.0000 1.0000 -0.1520 0.1520
#> 13        x        5 0.7453 0.0823  9.0536 0.0000  0.5840 0.9067
#> 14        m        5 0.2555 0.0925  2.7625 0.0057  0.0742 0.4368
#> 15        y        5 0.0000 0.0667  0.0000 1.0000 -0.1308 0.1308
confint(delta, level = 0.95)
#>    variable interval       2.5 %    97.5 %
#> 1         x        1  0.19615900 0.3677027
#> 2         m        1  0.43211639 0.6667453
#> 3         y        1 -0.10858822 0.1085882
#> 4         x        2  0.48422186 0.6972162
#> 5         m        2  0.45856163 0.7501522
#> 6         y        2 -0.15529725 0.1552973
#> 7         x        3  0.63646957 0.8866394
#> 8         m        3  0.33660726 0.6632909
#> 9         y        3 -0.16397530 0.1639753
#> 10        x        4  0.65330788 0.9424560
#> 11        m        4  0.19247845 0.5447742
#> 12        y        4 -0.15197006 0.1519701
#> 13        x        5  0.58398441 0.9066927
#> 14        m        5  0.07422134 0.4367724
#> 15        y        5 -0.13075816 0.1307582
plot(delta)



```

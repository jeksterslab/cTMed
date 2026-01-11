# Delta Method Sampling Variance-Covariance Matrix for the Standardized Total, Direct, and Indirect Effects of X on Y Through M Over a Specific Time Interval or a Range of Time Intervals

This function computes the delta method sampling variance-covariance
matrix for the standardized total, direct, and indirect effects of the
independent variable \\X\\ on the dependent variable \\Y\\ through
mediator variables \\\mathbf{m}\\ over a specific time interval \\\Delta
t\\ or a range of time intervals using the first-order stochastic
differential equation model's drift matrix \\\boldsymbol{\Phi}\\ and
process noise covariance matrix \\\boldsymbol{\Sigma}\\.

## Usage

``` r
DeltaMedStd(
  phi,
  sigma,
  vcov_theta,
  delta_t,
  from,
  to,
  med,
  ncores = NULL,
  tol = 0.01
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

Returns an object of class `ctmeddelta` which is a list with the
following elements:

- call:

  Function call.

- args:

  Function arguments.

- fun:

  Function used ("DeltaMedStd").

- output:

  A list the length of which is equal to the length of `delta_t`.

Each element in the `output` list has the following elements:

- delta_t:

  Time interval.

- jacobian:

  Jacobian matrix.

- est:

  Estimated standardized total, direct, and indirect effects.

- vcov:

  Sampling variance-covariance matrix of the estimated standardized
  total, direct, and indirect effects.

## Details

See
[`TotalStd()`](https://github.com/jeksterslab/cTMed/reference/TotalStd.md),
[`DirectStd()`](https://github.com/jeksterslab/cTMed/reference/DirectStd.md),
and
[`IndirectStd()`](https://github.com/jeksterslab/cTMed/reference/IndirectStd.md)
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
Modeling: A Multidisciplinary Journal, 23 (1), 61–75.
[doi:10.1080/10705511.2014.973960](https://doi.org/10.1080/10705511.2014.973960)

Pesigan, I. J. A., Russell, M. A., & Chow, S.-M. (2025). Inferences and
effect sizes for direct, indirect, and total effects in continuous-time
mediation models. Psychological Methods.
[doi:10.1037/met0000779](https://doi.org/10.1037/met0000779)

Ryan, O., & Hamaker, E. L. (2021). Time to intervene: A continuous-time
approach to network analysis and centrality. Psychometrika, 87 (1),
214–252.
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
DeltaMedStd(
  phi = phi,
  sigma = sigma,
  vcov_theta = vcov_theta,
  delta_t = 1,
  from = "x",
  to = "y",
  med = "m"
)
#> Call:
#> DeltaMedStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1, from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>     effect interval     est     se       z     p    2.5%   97.5%
#> 1    total        1 -0.1069 0.0345 -3.0977 0.002 -0.1745 -0.0393
#> 2   direct        1 -0.2858 0.0467 -6.1252 0.000 -0.3772 -0.1943
#> 3 indirect        1  0.1789 0.0200  8.9504 0.000  0.1397  0.2181

# Range of time intervals ---------------------------------------------------
delta <- DeltaMedStd(
  phi = phi,
  sigma = sigma,
  vcov_theta = vcov_theta,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m"
)
plot(delta)




# Methods -------------------------------------------------------------------
# DeltaMedStd has a number of methods including
# print, summary, confint, and plot
print(delta)
#> Call:
#> DeltaMedStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1:5, from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      effect interval     est     se       z      p    2.5%   97.5%
#> 1     total        1 -0.1069 0.0345 -3.0977 0.0020 -0.1745 -0.0393
#> 2    direct        1 -0.2858 0.0467 -6.1252 0.0000 -0.3772 -0.1943
#> 3  indirect        1  0.1789 0.0200  8.9504 0.0000  0.1397  0.2181
#> 4     total        2  0.0854 0.0351  2.4352 0.0149  0.0167  0.1541
#> 5    direct        2 -0.3429 0.0637 -5.3803 0.0000 -0.4678 -0.2180
#> 6  indirect        2  0.4283 0.0510  8.3988 0.0000  0.3283  0.5282
#> 7     total        3  0.2680 0.0324  8.2632 0.0000  0.2044  0.3316
#> 8    direct        3 -0.3114 0.0687 -4.5336 0.0000 -0.4460 -0.1768
#> 9  indirect        3  0.5794 0.0759  7.6315 0.0000  0.4306  0.7282
#> 10    total        4  0.3686 0.0346 10.6428 0.0000  0.3007  0.4364
#> 11   direct        4 -0.2537 0.0671 -3.7801 0.0002 -0.3852 -0.1221
#> 12 indirect        4  0.6222 0.0901  6.9059 0.0000  0.4456  0.7988
#> 13    total        5  0.3946 0.0392 10.0752 0.0000  0.3178  0.4713
#> 14   direct        5 -0.1954 0.0617 -3.1655 0.0015 -0.3163 -0.0744
#> 15 indirect        5  0.5899 0.0941  6.2696 0.0000  0.4055  0.7744
summary(delta)
#> Call:
#> DeltaMedStd(phi = phi, sigma = sigma, vcov_theta = vcov_theta, 
#>     delta_t = 1:5, from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      effect interval     est     se       z      p    2.5%   97.5%
#> 1     total        1 -0.1069 0.0345 -3.0977 0.0020 -0.1745 -0.0393
#> 2    direct        1 -0.2858 0.0467 -6.1252 0.0000 -0.3772 -0.1943
#> 3  indirect        1  0.1789 0.0200  8.9504 0.0000  0.1397  0.2181
#> 4     total        2  0.0854 0.0351  2.4352 0.0149  0.0167  0.1541
#> 5    direct        2 -0.3429 0.0637 -5.3803 0.0000 -0.4678 -0.2180
#> 6  indirect        2  0.4283 0.0510  8.3988 0.0000  0.3283  0.5282
#> 7     total        3  0.2680 0.0324  8.2632 0.0000  0.2044  0.3316
#> 8    direct        3 -0.3114 0.0687 -4.5336 0.0000 -0.4460 -0.1768
#> 9  indirect        3  0.5794 0.0759  7.6315 0.0000  0.4306  0.7282
#> 10    total        4  0.3686 0.0346 10.6428 0.0000  0.3007  0.4364
#> 11   direct        4 -0.2537 0.0671 -3.7801 0.0002 -0.3852 -0.1221
#> 12 indirect        4  0.6222 0.0901  6.9059 0.0000  0.4456  0.7988
#> 13    total        5  0.3946 0.0392 10.0752 0.0000  0.3178  0.4713
#> 14   direct        5 -0.1954 0.0617 -3.1655 0.0015 -0.3163 -0.0744
#> 15 indirect        5  0.5899 0.0941  6.2696 0.0000  0.4055  0.7744
confint(delta, level = 0.95)
#>      effect interval      2.5 %      97.5 %
#> 1     total        1 -0.1745281 -0.03925936
#> 2    direct        1 -0.3772269 -0.19433670
#> 3  indirect        1  0.1397151  0.21806096
#> 4     total        2  0.0166627  0.15408968
#> 5    direct        2 -0.4678058 -0.21798253
#> 6  indirect        2  0.3283280  0.52821273
#> 7     total        3  0.2044337  0.33156916
#> 8    direct        3 -0.4460439 -0.17678487
#> 9  indirect        3  0.4306074  0.72822429
#> 10    total        4  0.3006902  0.43643964
#> 11   direct        4 -0.3851778 -0.12213761
#> 12 indirect        4  0.4456289  0.79881634
#> 13    total        5  0.3178028  0.47131270
#> 14   direct        5 -0.3163438 -0.07440538
#> 15 indirect        5  0.4055114  0.77435334
plot(delta)



```

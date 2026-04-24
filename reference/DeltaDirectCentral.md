# Delta Method Sampling Variance-Covariance Matrix for the Direct Effect Centrality Over a Specific Time Interval or a Range of Time Intervals

This function computes the delta method sampling variance-covariance
matrix for the direct effect centrality over a specific time interval
\\\Delta t\\ or a range of time intervals using the first-order
stochastic differential equation model's drift matrix
\\\boldsymbol{\Phi}\\.

## Usage

``` r
DeltaDirectCentral(phi, vcov_phi_vec, delta_t, ncores = NULL, tol = 0.01)
```

## Arguments

- phi:

  Numeric matrix. The drift matrix (\\\boldsymbol{\Phi}\\). `phi` should
  have row and column names pertaining to the variables in the system.

- vcov_phi_vec:

  Numeric matrix. The sampling variance-covariance matrix of
  \\\mathrm{vec} \left( \boldsymbol{\Phi} \right)\\.

- delta_t:

  Vector of positive numbers. Time interval (\\\Delta t\\).

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

  Function used ("DeltaDirectCentral").

- output:

  A list the length of which is equal to the length of `delta_t`.

Each element in the `output` list has the following elements:

- delta_t:

  Time interval.

- jacobian:

  Jacobian matrix.

- est:

  Estimated direct effect centrality.

- vcov:

  Sampling variance-covariance matrix of estimated direct effect
  centrality.

## Details

See
[`DirectCentral()`](https://github.com/jeksterslab/cTMed/reference/DirectCentral.md)
more details.

### Delta Method

Let \\\boldsymbol{\theta}\\ be \\\mathrm{vec} \left( \boldsymbol{\Phi}
\right)\\, that is, the elements of the \\\boldsymbol{\Phi}\\ matrix in
vector form sorted column-wise. Let \\\hat{\boldsymbol{\theta}}\\ be
\\\mathrm{vec} \left( \hat{\boldsymbol{\Phi}} \right)\\. By the
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
    0.002704274, -0.001475275, 0.000949122,
    -0.001619422, 0.000885122, -0.000569404,
    0.00085493, -0.000465824, 0.000297815,
    -0.001475275, 0.004428442, -0.002642303,
    0.000980573, -0.00271817, 0.001618805,
    -0.000586921, 0.001478421, -0.000871547,
    0.000949122, -0.002642303, 0.006402668,
    -0.000697798, 0.001813471, -0.004043138,
    0.000463086, -0.001120949, 0.002271711,
    -0.001619422, 0.000980573, -0.000697798,
    0.002079286, -0.001152501, 0.000753,
    -0.001528701, 0.000820587, -0.000517524,
    0.000885122, -0.00271817, 0.001813471,
    -0.001152501, 0.00342605, -0.002075005,
    0.000899165, -0.002532849, 0.001475579,
    -0.000569404, 0.001618805, -0.004043138,
    0.000753, -0.002075005, 0.004984032,
    -0.000622255, 0.001634917, -0.003705661,
    0.00085493, -0.000586921, 0.000463086,
    -0.001528701, 0.000899165, -0.000622255,
    0.002060076, -0.001096684, 0.000686386,
    -0.000465824, 0.001478421, -0.001120949,
    0.000820587, -0.002532849, 0.001634917,
    -0.001096684, 0.003328692, -0.001926088,
    0.000297815, -0.000871547, 0.002271711,
    -0.000517524, 0.001475579, -0.003705661,
    0.000686386, -0.001926088, 0.004726235
  ),
  nrow = 9
)

# Specific time interval ----------------------------------------------------
DeltaDirectCentral(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  delta_t = 1
)
#> Call:
#> DeltaDirectCentral(phi = phi, vcov_phi_vec = vcov_phi_vec, delta_t = 1)
#> 
#> Direct Effect Centrality
#>   variable interval     est     se       z p    2.5%   97.5%
#> 1        x        1  0.3998 0.0433  9.2421 0  0.3150  0.4846
#> 2        m        1 -0.2675 0.0526 -5.0888 0 -0.3705 -0.1644
#> 3        y        1  0.5000 0.0444 11.2500 0  0.4129  0.5871

# Range of time intervals ---------------------------------------------------
delta <- DeltaDirectCentral(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  delta_t = 1:5
)
plot(delta)




# Methods -------------------------------------------------------------------
# DeltaDirectCentral has a number of methods including
# print, summary, confint, and plot
print(delta)
#> Call:
#> DeltaDirectCentral(phi = phi, vcov_phi_vec = vcov_phi_vec, delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se       z p    2.5%   97.5%
#> 1         x        1  0.3998 0.0433  9.2421 0  0.3150  0.4846
#> 2         m        1 -0.2675 0.0526 -5.0888 0 -0.3705 -0.1644
#> 3         y        1  0.5000 0.0444 11.2500 0  0.4129  0.5871
#> 4         x        2  0.4398 0.0390 11.2768 0  0.3634  0.5162
#> 5         m        2 -0.3209 0.0609 -5.2687 0 -0.4403 -0.2015
#> 6         y        2  0.6499 0.0498 13.0510 0  0.5523  0.7475
#> 7         x        3  0.3638 0.0338 10.7767 0  0.2977  0.4300
#> 8         m        3 -0.2914 0.0569 -5.1238 0 -0.4029 -0.1800
#> 9         y        3  0.6347 0.0537 11.8162 0  0.5294  0.7400
#> 10        x        4  0.2683 0.0311  8.6324 0  0.2074  0.3292
#> 11        m        4 -0.2374 0.0502 -4.7322 0 -0.3357 -0.1391
#> 12        y        4  0.5521 0.0594  9.2971 0  0.4357  0.6685
#> 13        x        5  0.1859 0.0281  6.6178 0  0.1309  0.2410
#> 14        m        5 -0.1828 0.0432 -4.2317 0 -0.2675 -0.0982
#> 15        y        5  0.4511 0.0632  7.1345 0  0.3272  0.5750
summary(delta)
#> Call:
#> DeltaDirectCentral(phi = phi, vcov_phi_vec = vcov_phi_vec, delta_t = 1:5)
#> 
#> Direct Effect Centrality
#>    variable interval     est     se       z p    2.5%   97.5%
#> 1         x        1  0.3998 0.0433  9.2421 0  0.3150  0.4846
#> 2         m        1 -0.2675 0.0526 -5.0888 0 -0.3705 -0.1644
#> 3         y        1  0.5000 0.0444 11.2500 0  0.4129  0.5871
#> 4         x        2  0.4398 0.0390 11.2768 0  0.3634  0.5162
#> 5         m        2 -0.3209 0.0609 -5.2687 0 -0.4403 -0.2015
#> 6         y        2  0.6499 0.0498 13.0510 0  0.5523  0.7475
#> 7         x        3  0.3638 0.0338 10.7767 0  0.2977  0.4300
#> 8         m        3 -0.2914 0.0569 -5.1238 0 -0.4029 -0.1800
#> 9         y        3  0.6347 0.0537 11.8162 0  0.5294  0.7400
#> 10        x        4  0.2683 0.0311  8.6324 0  0.2074  0.3292
#> 11        m        4 -0.2374 0.0502 -4.7322 0 -0.3357 -0.1391
#> 12        y        4  0.5521 0.0594  9.2971 0  0.4357  0.6685
#> 13        x        5  0.1859 0.0281  6.6178 0  0.1309  0.2410
#> 14        m        5 -0.1828 0.0432 -4.2317 0 -0.2675 -0.0982
#> 15        y        5  0.4511 0.0632  7.1345 0  0.3272  0.5750
confint(delta, level = 0.95)
#>    variable interval      2.5 %      97.5 %
#> 1         x        1  0.3150424  0.48462884
#> 2         m        1 -0.3704643 -0.16444345
#> 3         y        1  0.4129184  0.58714981
#> 4         x        2  0.3633661  0.51624749
#> 5         m        2 -0.4402798 -0.20152725
#> 6         y        2  0.5522816  0.74747496
#> 7         x        3  0.2976571  0.42999569
#> 8         m        3 -0.4029265 -0.17995872
#> 9         y        3  0.5294357  0.73999726
#> 10        x        4  0.2073515  0.32916710
#> 11        m        4 -0.3357123 -0.13906775
#> 12        y        4  0.4357156  0.66850042
#> 13        x        5  0.1308653  0.24099861
#> 14        m        5 -0.2675316 -0.09815781
#> 15        y        5  0.3271815  0.57503700
plot(delta)



```

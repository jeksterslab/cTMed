# Indirect Effect of X on Y Through M Over a Specific Time Interval

This function computes the indirect effect of the independent variable
\\X\\ on the dependent variable \\Y\\ through mediator variables
\\\mathbf{m}\\ over a specific time interval \\\Delta t\\ using the
first-order stochastic differential equation model's drift matrix
\\\boldsymbol{\Phi}\\.

## Usage

``` r
Indirect(phi, delta_t, from, to, med)
```

## Arguments

- phi:

  Numeric matrix. The drift matrix (\\\boldsymbol{\Phi}\\). `phi` should
  have row and column names pertaining to the variables in the system.

- delta_t:

  Numeric. Time interval (\\\Delta t\\).

- from:

  Character string. Name of the independent variable \\X\\ in `phi`.

- to:

  Character string. Name of the dependent variable \\Y\\ in `phi`.

- med:

  Character vector. Name/s of the mediator variable/s in `phi`.

## Value

Returns an object of class `ctmedeffect` which is a list with the
following elements:

- call:

  Function call.

- args:

  Function arguments.

- fun:

  Function used ("Indirect").

- output:

  The indirect effect.

## Details

The indirect effect of the independent variable \\X\\ on the dependent
variable \\Y\\ relative to some mediator variables \\\mathbf{m}\\ over a
specific time interval \\\Delta t\\ is given by \$\$
\mathrm{Indirect}\_{{\Delta t}\_{i, j}} = \exp \left( \Delta t
\boldsymbol{\Phi} \right)\_{i, j} - \exp \left( \Delta t
\mathbf{D}\_{\mathbf{m}} \boldsymbol{\Phi} \mathbf{D}\_{\mathbf{m}}
\right)\_{i, j} \$\$ where \\\boldsymbol{\Phi}\\ denotes the drift
matrix, \\\mathbf{D}\_{\mathbf{m}}\\ a matrix where the off diagonal
elements are zeros and the diagonal elements are zero for the
index/indices of mediator variables \\\mathbf{m}\\ and one otherwise,
\\i\\ the row index of \\Y\\ in \\\boldsymbol{\Phi}\\, \\j\\ the column
index of \\X\\ in \\\boldsymbol{\Phi}\\, and \\\Delta t\\ the time
interval.

### Linear Stochastic Differential Equation Model

The measurement model is given by \$\$ \mathbf{y}\_{i, t} =
\boldsymbol{\nu} + \boldsymbol{\Lambda} \boldsymbol{\eta}\_{i, t} +
\boldsymbol{\varepsilon}\_{i, t}, \quad \mathrm{with} \quad
\boldsymbol{\varepsilon}\_{i, t} \sim \mathcal{N} \left( \mathbf{0},
\boldsymbol{\Theta} \right) \$\$ where \\\mathbf{y}\_{i, t}\\,
\\\boldsymbol{\eta}\_{i, t}\\, and \\\boldsymbol{\varepsilon}\_{i, t}\\
are random variables and \\\boldsymbol{\nu}\\, \\\boldsymbol{\Lambda}\\,
and \\\boldsymbol{\Theta}\\ are model parameters. \\\mathbf{y}\_{i, t}\\
represents a vector of observed random variables,
\\\boldsymbol{\eta}\_{i, t}\\ a vector of latent random variables, and
\\\boldsymbol{\varepsilon}\_{i, t}\\ a vector of random measurement
errors, at time \\t\\ and individual \\i\\. \\\boldsymbol{\nu}\\ denotes
a vector of intercepts, \\\boldsymbol{\Lambda}\\ a matrix of factor
loadings, and \\\boldsymbol{\Theta}\\ the covariance matrix of
\\\boldsymbol{\varepsilon}\\.

An alternative representation of the measurement error is given by \$\$
\boldsymbol{\varepsilon}\_{i, t} = \boldsymbol{\Theta}^{\frac{1}{2}}
\mathbf{z}\_{i, t}, \quad \mathrm{with} \quad \mathbf{z}\_{i, t} \sim
\mathcal{N} \left( \mathbf{0}, \mathbf{I} \right) \$\$ where
\\\mathbf{z}\_{i, t}\\ is a vector of independent standard normal random
variables and \\ \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right) \left(
\boldsymbol{\Theta}^{\frac{1}{2}} \right)^{\prime} = \boldsymbol{\Theta}
. \\

The dynamic structure is given by \$\$ \mathrm{d} \boldsymbol{\eta}\_{i,
t} = \left( \boldsymbol{\iota} + \boldsymbol{\Phi}
\boldsymbol{\eta}\_{i, t} \right) \mathrm{d}t +
\boldsymbol{\Sigma}^{\frac{1}{2}} \mathrm{d} \mathbf{W}\_{i, t} \$\$
where \\\boldsymbol{\iota}\\ is a term which is unobserved and constant
over time, \\\boldsymbol{\Phi}\\ is the drift matrix which represents
the rate of change of the solution in the absence of any random
fluctuations, \\\boldsymbol{\Sigma}\\ is the matrix of volatility or
randomness in the process, and \\\mathrm{d}\boldsymbol{W}\\ is a Wiener
process or Brownian motion, which represents random fluctuations.

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
[`DeltaMedStd()`](https://github.com/jeksterslab/cTMed/reference/DeltaMedStd.md),
[`DeltaTotalCentral()`](https://github.com/jeksterslab/cTMed/reference/DeltaTotalCentral.md),
[`Direct()`](https://github.com/jeksterslab/cTMed/reference/Direct.md),
[`DirectStd()`](https://github.com/jeksterslab/cTMed/reference/DirectStd.md),
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
delta_t <- 1
Indirect(
  phi = phi,
  delta_t = delta_t,
  from = "x",
  to = "y",
  med = "m"
)
#> [1] 0.1674
phi <- matrix(
  data = c(
    -6, 5.5, 0, 0,
    1.25, -2.5, 5.9, -7.3,
    0, 0, -6, 2.5,
    5, 0, 0, -6
  ),
  nrow = 4
)
colnames(phi) <- rownames(phi) <- paste0("y", 1:4)
Indirect(
  phi = phi,
  delta_t = delta_t,
  from = "y2",
  to = "y4",
  med = c("y1", "y3")
)
#> [1] 0.2427
```

# Monte Carlo Method Confidence Intervals

Monte Carlo Method Confidence Intervals

## Usage

``` r
# S3 method for class 'ctmedmc'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `ctmedmc`.

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- ...:

  additional arguments.

## Value

Returns a data frame of confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
set.seed(42)
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

# Specific time interval ----------------------------------------------------
mc <- MCMed(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  delta_t = 1,
  from = "x",
  to = "y",
  med = "m",
  R = 100L # use a large value for R in actual research
)
confint(mc, level = 0.95)
#>     effect interval      2.5 %      97.5 %
#> 1    total        1 -0.1727980 -0.03444335
#> 2   direct        1 -0.3530971 -0.18029584
#> 3 indirect        1  0.1310468  0.19907046

# Range of time intervals ---------------------------------------------------
mc <- MCMed(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m",
  R = 100L # use a large value for R in actual research
)
confint(mc, level = 0.95)
#>      effect interval       2.5 %      97.5 %
#> 1     total        1 -0.15232781 -0.04123191
#> 2    direct        1 -0.34098924 -0.19575030
#> 3  indirect        1  0.13034214  0.19730024
#> 4     total        2  0.01734713  0.13816481
#> 5    direct        2 -0.43347383 -0.22496835
#> 6  indirect        2  0.32090375  0.48438931
#> 7     total        3  0.17891544  0.31595326
#> 8    direct        3 -0.42496332 -0.19529856
#> 9  indirect        3  0.43134204  0.68245524
#> 10    total        4  0.27762014  0.42528849
#> 11   direct        4 -0.37132486 -0.14987686
#> 12 indirect        4  0.45427525  0.74867661
#> 13    total        5  0.30966378  0.47707320
#> 14   direct        5 -0.31560524 -0.11390776
#> 15 indirect        5  0.42321469  0.75325267
```

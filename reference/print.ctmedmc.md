# Print Method for Object of Class `ctmedmc`

Print Method for Object of Class `ctmedmc`

## Usage

``` r
# S3 method for class 'ctmedmc'
print(x, alpha = 0.05, digits = 4, ...)
```

## Arguments

- x:

  an object of class `ctmedmc`.

- alpha:

  Numeric vector. Significance level \\\alpha\\.

- digits:

  Integer indicating the number of decimal places to display.

- ...:

  further arguments.

## Value

Prints a list of matrices of time intervals, estimates, standard errors,
number of Monte Carlo replications, and confidence intervals.

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
print(mc)
#> Call:
#> MCMed(phi = phi, vcov_phi_vec = vcov_phi_vec, delta_t = 1, from = "x", 
#>     to = "y", med = "m", R = 100L)
#> 
#> Total, Direct, and Indirect Effects
#> 
#>     effect interval     est     se   R    2.5%   97.5%
#> 1    total        1 -0.1000 0.0349 100 -0.1728 -0.0344
#> 2   direct        1 -0.2675 0.0454 100 -0.3531 -0.1803
#> 3 indirect        1  0.1674 0.0189 100  0.1310  0.1991

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
print(mc)
#> Call:
#> MCMed(phi = phi, vcov_phi_vec = vcov_phi_vec, delta_t = 1:5, 
#>     from = "x", to = "y", med = "m", R = 100L)
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      effect interval     est     se   R    2.5%   97.5%
#> 1     total        1 -0.1000 0.0283 100 -0.1523 -0.0412
#> 2    direct        1 -0.2675 0.0379 100 -0.3410 -0.1958
#> 3  indirect        1  0.1674 0.0181 100  0.1303  0.1973
#> 4     total        2  0.0799 0.0318 100  0.0173  0.1382
#> 5    direct        2 -0.3209 0.0533 100 -0.4335 -0.2250
#> 6  indirect        2  0.4008 0.0472 100  0.3209  0.4844
#> 7     total        3  0.2508 0.0341 100  0.1789  0.3160
#> 8    direct        3 -0.2914 0.0600 100 -0.4250 -0.1953
#> 9  indirect        3  0.5423 0.0723 100  0.4313  0.6825
#> 10    total        4  0.3449 0.0396 100  0.2776  0.4253
#> 11   direct        4 -0.2374 0.0614 100 -0.3713 -0.1499
#> 12 indirect        4  0.5823 0.0888 100  0.4543  0.7487
#> 13    total        5  0.3693 0.0454 100  0.3097  0.4771
#> 14   direct        5 -0.1828 0.0592 100 -0.3156 -0.1139
#> 15 indirect        5  0.5521 0.0964 100  0.4232  0.7533
```

# Print Method for Object of Class `ctmeddelta`

Print Method for Object of Class `ctmeddelta`

## Usage

``` r
# S3 method for class 'ctmeddelta'
print(x, alpha = 0.05, digits = 4, ...)
```

## Arguments

- x:

  an object of class `ctmeddelta`.

- alpha:

  Numeric vector. Significance level \\\alpha\\.

- digits:

  Integer indicating the number of decimal places to display.

- ...:

  further arguments.

## Value

Prints a list of matrices of time intervals, estimates, standard errors,
test statistics, p-values, and confidence intervals.

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

# Specific time interval ----------------------------------------------------
delta <- DeltaMed(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  delta_t = 1,
  from = "x",
  to = "y",
  med = "m"
)
print(delta)
#> Call:
#> DeltaMed(phi = phi, vcov_phi_vec = vcov_phi_vec, delta_t = 1, 
#>     from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>     effect interval     est     se       z      p    2.5%   97.5%
#> 1    total        1 -0.1000 0.0306 -3.2703 0.0011 -0.1600 -0.0401
#> 2   direct        1 -0.2675 0.0394 -6.7894 0.0000 -0.3447 -0.1902
#> 3 indirect        1  0.1674 0.0175  9.5524 0.0000  0.1331  0.2018

# Range of time intervals ---------------------------------------------------
delta <- DeltaMed(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m"
)
print(delta)
#> Call:
#> DeltaMed(phi = phi, vcov_phi_vec = vcov_phi_vec, delta_t = 1:5, 
#>     from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      effect interval     est     se       z      p    2.5%   97.5%
#> 1     total        1 -0.1000 0.0306 -3.2703 0.0011 -0.1600 -0.0401
#> 2    direct        1 -0.2675 0.0394 -6.7894 0.0000 -0.3447 -0.1902
#> 3  indirect        1  0.1674 0.0175  9.5524 0.0000  0.1331  0.2018
#> 4     total        2  0.0799 0.0342  2.3337 0.0196  0.0128  0.1470
#> 5    direct        2 -0.3209 0.0552 -5.8129 0.0000 -0.4291 -0.2127
#> 6  indirect        2  0.4008 0.0461  8.7033 0.0000  0.3105  0.4911
#> 7     total        3  0.2508 0.0353  7.1106 0.0000  0.1817  0.3199
#> 8    direct        3 -0.2914 0.0608 -4.7907 0.0000 -0.4107 -0.1722
#> 9  indirect        3  0.5423 0.0703  7.7121 0.0000  0.4044  0.6801
#> 10    total        4  0.3449 0.0394  8.7512 0.0000  0.2677  0.4222
#> 11   direct        4 -0.2374 0.0604 -3.9333 0.0001 -0.3557 -0.1191
#> 12 indirect        4  0.5823 0.0850  6.8529 0.0000  0.4158  0.7489
#> 13    total        5  0.3693 0.0441  8.3649 0.0000  0.2827  0.4558
#> 14   direct        5 -0.1828 0.0561 -3.2606 0.0011 -0.2928 -0.0729
#> 15 indirect        5  0.5521 0.0899  6.1417 0.0000  0.3759  0.7283
```

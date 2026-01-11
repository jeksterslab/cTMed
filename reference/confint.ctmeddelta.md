# Delta Method Confidence Intervals

Delta Method Confidence Intervals

## Usage

``` r
# S3 method for class 'ctmeddelta'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `ctmeddelta`.

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
confint(delta, level = 0.95)
#>     effect interval      2.5 %      97.5 %
#> 1    total        1 -0.1599945 -0.04008223
#> 2   direct        1 -0.3446621 -0.19024569
#> 3 indirect        1  0.1330653  0.20176572

# Range of time intervals ---------------------------------------------------
delta <- DeltaMed(
  phi = phi,
  vcov_phi_vec = vcov_phi_vec,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m"
)
confint(delta, level = 0.95)
#>      effect interval       2.5 %      97.5 %
#> 1     total        1 -0.15999452 -0.04008223
#> 2    direct        1 -0.34466208 -0.19024569
#> 3  indirect        1  0.13306530  0.20176572
#> 4     total        2  0.01279611  0.14700550
#> 5    direct        2 -0.42910491 -0.21270209
#> 6  indirect        2  0.31054364  0.49106497
#> 7     total        3  0.18167990  0.31994775
#> 8    direct        3 -0.41067673 -0.17220847
#> 9  indirect        3  0.40444738  0.68006547
#> 10    total        4  0.26767567  0.42218015
#> 11   direct        4 -0.35568031 -0.11909972
#> 12 indirect        4  0.41577174  0.74886411
#> 13    total        5  0.28273472  0.45577286
#> 14   direct        5 -0.29275471 -0.07293472
#> 15 indirect        5  0.37591173  0.72828528
```

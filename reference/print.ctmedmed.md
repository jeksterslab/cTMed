# Print Method for Object of Class `ctmedmed`

Print Method for Object of Class `ctmedmed`

## Usage

``` r
# S3 method for class 'ctmedmed'
print(x, digits = 4, ...)
```

## Arguments

- x:

  an object of class `ctmedmed`.

- digits:

  Integer indicating the number of decimal places to display.

- ...:

  further arguments.

## Value

Prints a matrix of effects.

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

# Specific time interval ----------------------------------------------------
med <- Med(
  phi = phi,
  delta_t = 1,
  from = "x",
  to = "y",
  med = "m"
)
print(med)
#> Call:
#> Med(phi = phi, delta_t = 1, from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      interval total  direct indirect
#> [1,]        1  -0.1 -0.2675   0.1674

# Range of time intervals ---------------------------------------------------
med <- Med(
  phi = phi,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m"
)
print(med)
#> Call:
#> Med(phi = phi, delta_t = 1:5, from = "x", to = "y", med = "m")
#> 
#> Total, Direct, and Indirect Effects
#> 
#>      interval   total  direct indirect
#> [1,]        1 -0.1000 -0.2675   0.1674
#> [2,]        2  0.0799 -0.3209   0.4008
#> [3,]        3  0.2508 -0.2914   0.5423
#> [4,]        4  0.3449 -0.2374   0.5823
#> [5,]        5  0.3693 -0.1828   0.5521
```

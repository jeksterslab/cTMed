# Print Method for Object of Class `ctmedeffect`

Print Method for Object of Class `ctmedeffect`

## Usage

``` r
# S3 method for class 'ctmedeffect'
print(x, digits = 4, ...)
```

## Arguments

- x:

  an object of class `ctmedeffect`.

- digits:

  Integer indicating the number of decimal places to display.

- ...:

  further arguments.

## Value

Prints the effects.

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

# Time Interval of One -----------------------------------------------------

## Total Effect ------------------------------------------------------------
total_dt <- Total(
  phi = phi,
  delta_t = delta_t
)
print(total_dt)
#>         x      m      y
#> x  0.6998 0.0000 0.0000
#> m  0.5000 0.5999 0.0000
#> y -0.1000 0.3998 0.5001

## Direct Effect -----------------------------------------------------------
direct_dt <- Direct(
  phi = phi,
  delta_t = delta_t,
  from = "x",
  to = "y",
  med = "m"
)
print(direct_dt)
#> [1] -0.2675

## Indirect Effect ---------------------------------------------------------
indirect_dt <- Indirect(
  phi = phi,
  delta_t = delta_t,
  from = "x",
  to = "y",
  med = "m"
)
print(indirect_dt)
#> [1] 0.1674
```

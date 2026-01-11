# Plot Method for an Object of Class `ctmedtraj`

Plot Method for an Object of Class `ctmedtraj`

## Usage

``` r
# S3 method for class 'ctmedtraj'
plot(x, legend_pos = "topright", total = TRUE, ...)
```

## Arguments

- x:

  Object of class `ctmedtraj`.

- legend_pos:

  Character vector. Optional argument. Legend position.

- total:

  Logical. If `total = TRUE`, include the total effect trajectory. If
  `total = FALSE`, exclude the total effect trajectory.

- ...:

  Additional arguments.

## Value

Displays trajectory plots of the effects.

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

traj <- Trajectory(
  mu0 = c(3, 3, -3),
  time = 150,
  phi = phi,
  med = "m"
)

plot(traj)



```

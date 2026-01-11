# Simulate Trajectories of Variables

This function simulates trajectories of variables without measurement
error or process noise. `Total` corresponds to the total effect and
`Direct` corresponds to the portion of the total effect where the
indirect effect is removed.

## Usage

``` r
Trajectory(mu0, time, phi, med)
```

## Arguments

- mu0:

  Numeric vector. Initial values of the variables.

- time:

  Positive integer. Number of time points.

- phi:

  Numeric matrix. The drift matrix (\\\boldsymbol{\Phi}\\). `phi` should
  have row and column names pertaining to the variables in the system.

- med:

  Character vector. Name/s of the mediator variable/s in `phi`.

## Value

Returns an object of class `ctmedtraj` which is a list with the
following elements:

- call:

  Function call.

- args:

  Function arguments.

- fun:

  Function used ("Trajectory").

- output:

  A data frame of simulated data.

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
[`TotalStd()`](https://github.com/jeksterslab/cTMed/reference/TotalStd.md)

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




# Methods -------------------------------------------------------------------
# Trajectory has a number of methods including
# print, summary, and plot

traj <- Trajectory(
  mu0 = c(3, 3, -3),
  time = 25,
  phi = phi,
  med = "m"
)
print(traj)
#> Call:
#> Trajectory(mu0 = c(3, 3, -3), time = 25, phi = phi, med = "m")
#>    time     y1     y2      y3   effect
#> 1   0.0 3.0000 3.0000 -3.0000    total
#> 2   0.1 2.8948 3.0720 -2.7133    total
#> 3   0.2 2.7933 3.1327 -2.4367    total
#> 4   0.3 2.6953 3.1829 -2.1703    total
#> 5   0.4 2.6008 3.2233 -1.9144    total
#> 6   0.5 2.5096 3.2547 -1.6691    total
#> 7   0.6 2.4216 3.2779 -1.4344    total
#> 8   0.7 2.3366 3.2933 -1.2103    total
#> 9   0.8 2.2547 3.3018 -0.9968    total
#> 10  0.9 2.1756 3.3038 -0.7936    total
#> 11  1.0 2.0993 3.2998 -0.6008    total
#> 12  1.1 2.0257 3.2904 -0.4181    total
#> 13  1.2 1.9547 3.2760 -0.2454    total
#> 14  1.3 1.8861 3.2571 -0.0823    total
#> 15  1.4 1.8200 3.2341  0.0713    total
#> 16  1.5 1.7561 3.2074  0.2156    total
#> 17  1.6 1.6945 3.1772  0.3510    total
#> 18  1.7 1.6351 3.1441  0.4778    total
#> 19  1.8 1.5778 3.1081  0.5962    total
#> 20  1.9 1.5224 3.0698  0.7064    total
#> 21  2.0 1.4690 3.0293  0.8089    total
#> 22  2.1 1.4175 2.9868  0.9039    total
#> 23  2.2 1.3678 2.9427  0.9916    total
#> 24  2.3 1.3198 2.8971  1.0725    total
#> 25  2.4 1.2736 2.8502  1.1467    total
#> 26  0.0 3.0000 3.0000 -3.0000   direct
#> 27  0.1 2.8948 2.8506 -2.9272   direct
#> 28  0.2 2.7933 2.7085 -2.8549   direct
#> 29  0.3 2.6953 2.5736 -2.7830   direct
#> 30  0.4 2.6008 2.4454 -2.7118   direct
#> 31  0.5 2.5096 2.3236 -2.6412   direct
#> 32  0.6 2.4216 2.2078 -2.5716   direct
#> 33  0.7 2.3366 2.0978 -2.5028   direct
#> 34  0.8 2.2547 1.9933 -2.4350   direct
#> 35  0.9 2.1756 1.8940 -2.3682   direct
#> 36  1.0 2.0993 1.7997 -2.3026   direct
#> 37  1.1 2.0257 1.7100 -2.2381   direct
#> 38  1.2 1.9547 1.6248 -2.1747   direct
#> 39  1.3 1.8861 1.5439 -2.1126   direct
#> 40  1.4 1.8200 1.4670 -2.0517   direct
#> 41  1.5 1.7561 1.3939 -1.9920   direct
#> 42  1.6 1.6945 1.3245 -1.9336   direct
#> 43  1.7 1.6351 1.2585 -1.8765   direct
#> 44  1.8 1.5778 1.1958 -1.8207   direct
#> 45  1.9 1.5224 1.1362 -1.7662   direct
#> 46  2.0 1.4690 1.0796 -1.7129   direct
#> 47  2.1 1.4175 1.0258 -1.6610   direct
#> 48  2.2 1.3678 0.9747 -1.6103   direct
#> 49  2.3 1.3198 0.9262 -1.5609   direct
#> 50  2.4 1.2736 0.8800 -1.5127   direct
#> 51  0.0 3.0000 3.0000 -3.0000 indirect
#> 52  0.1 2.8948 3.0720 -2.5852 indirect
#> 53  0.2 2.7933 3.1327 -2.1935 indirect
#> 54  0.3 2.6953 3.1829 -1.8242 indirect
#> 55  0.4 2.6008 3.2233 -1.4764 indirect
#> 56  0.5 2.5096 3.2547 -1.1493 indirect
#> 57  0.6 2.4216 3.2779 -0.8423 indirect
#> 58  0.7 2.3366 3.2933 -0.5544 indirect
#> 59  0.8 2.2547 3.3018 -0.2850 indirect
#> 60  0.9 2.1756 3.3038 -0.0333 indirect
#> 61  1.0 2.0993 3.2998  0.2015 indirect
#> 62  1.1 2.0257 3.2904  0.4201 indirect
#> 63  1.2 1.9547 3.2760  0.6233 indirect
#> 64  1.3 1.8861 3.2571  0.8116 indirect
#> 65  1.4 1.8200 3.2341  0.9859 indirect
#> 66  1.5 1.7561 3.2074  1.1467 indirect
#> 67  1.6 1.6945 3.1772  1.2948 indirect
#> 68  1.7 1.6351 3.1441  1.4307 indirect
#> 69  1.8 1.5778 3.1081  1.5551 indirect
#> 70  1.9 1.5224 3.0698  1.6685 indirect
#> 71  2.0 1.4690 3.0293  1.7716 indirect
#> 72  2.1 1.4175 2.9868  1.8649 indirect
#> 73  2.2 1.3678 2.9427  1.9488 indirect
#> 74  2.3 1.3198 2.8971  2.0240 indirect
#> 75  2.4 1.2736 2.8502  2.0908 indirect
summary(traj)
#> Call:
#> Trajectory(mu0 = c(3, 3, -3), time = 25, phi = phi, med = "m")
#>    time     y1     y2      y3   effect
#> 1   0.0 3.0000 3.0000 -3.0000    total
#> 2   0.1 2.8948 3.0720 -2.7133    total
#> 3   0.2 2.7933 3.1327 -2.4367    total
#> 4   0.3 2.6953 3.1829 -2.1703    total
#> 5   0.4 2.6008 3.2233 -1.9144    total
#> 6   0.5 2.5096 3.2547 -1.6691    total
#> 7   0.6 2.4216 3.2779 -1.4344    total
#> 8   0.7 2.3366 3.2933 -1.2103    total
#> 9   0.8 2.2547 3.3018 -0.9968    total
#> 10  0.9 2.1756 3.3038 -0.7936    total
#> 11  1.0 2.0993 3.2998 -0.6008    total
#> 12  1.1 2.0257 3.2904 -0.4181    total
#> 13  1.2 1.9547 3.2760 -0.2454    total
#> 14  1.3 1.8861 3.2571 -0.0823    total
#> 15  1.4 1.8200 3.2341  0.0713    total
#> 16  1.5 1.7561 3.2074  0.2156    total
#> 17  1.6 1.6945 3.1772  0.3510    total
#> 18  1.7 1.6351 3.1441  0.4778    total
#> 19  1.8 1.5778 3.1081  0.5962    total
#> 20  1.9 1.5224 3.0698  0.7064    total
#> 21  2.0 1.4690 3.0293  0.8089    total
#> 22  2.1 1.4175 2.9868  0.9039    total
#> 23  2.2 1.3678 2.9427  0.9916    total
#> 24  2.3 1.3198 2.8971  1.0725    total
#> 25  2.4 1.2736 2.8502  1.1467    total
#> 26  0.0 3.0000 3.0000 -3.0000   direct
#> 27  0.1 2.8948 2.8506 -2.9272   direct
#> 28  0.2 2.7933 2.7085 -2.8549   direct
#> 29  0.3 2.6953 2.5736 -2.7830   direct
#> 30  0.4 2.6008 2.4454 -2.7118   direct
#> 31  0.5 2.5096 2.3236 -2.6412   direct
#> 32  0.6 2.4216 2.2078 -2.5716   direct
#> 33  0.7 2.3366 2.0978 -2.5028   direct
#> 34  0.8 2.2547 1.9933 -2.4350   direct
#> 35  0.9 2.1756 1.8940 -2.3682   direct
#> 36  1.0 2.0993 1.7997 -2.3026   direct
#> 37  1.1 2.0257 1.7100 -2.2381   direct
#> 38  1.2 1.9547 1.6248 -2.1747   direct
#> 39  1.3 1.8861 1.5439 -2.1126   direct
#> 40  1.4 1.8200 1.4670 -2.0517   direct
#> 41  1.5 1.7561 1.3939 -1.9920   direct
#> 42  1.6 1.6945 1.3245 -1.9336   direct
#> 43  1.7 1.6351 1.2585 -1.8765   direct
#> 44  1.8 1.5778 1.1958 -1.8207   direct
#> 45  1.9 1.5224 1.1362 -1.7662   direct
#> 46  2.0 1.4690 1.0796 -1.7129   direct
#> 47  2.1 1.4175 1.0258 -1.6610   direct
#> 48  2.2 1.3678 0.9747 -1.6103   direct
#> 49  2.3 1.3198 0.9262 -1.5609   direct
#> 50  2.4 1.2736 0.8800 -1.5127   direct
#> 51  0.0 3.0000 3.0000 -3.0000 indirect
#> 52  0.1 2.8948 3.0720 -2.5852 indirect
#> 53  0.2 2.7933 3.1327 -2.1935 indirect
#> 54  0.3 2.6953 3.1829 -1.8242 indirect
#> 55  0.4 2.6008 3.2233 -1.4764 indirect
#> 56  0.5 2.5096 3.2547 -1.1493 indirect
#> 57  0.6 2.4216 3.2779 -0.8423 indirect
#> 58  0.7 2.3366 3.2933 -0.5544 indirect
#> 59  0.8 2.2547 3.3018 -0.2850 indirect
#> 60  0.9 2.1756 3.3038 -0.0333 indirect
#> 61  1.0 2.0993 3.2998  0.2015 indirect
#> 62  1.1 2.0257 3.2904  0.4201 indirect
#> 63  1.2 1.9547 3.2760  0.6233 indirect
#> 64  1.3 1.8861 3.2571  0.8116 indirect
#> 65  1.4 1.8200 3.2341  0.9859 indirect
#> 66  1.5 1.7561 3.2074  1.1467 indirect
#> 67  1.6 1.6945 3.1772  1.2948 indirect
#> 68  1.7 1.6351 3.1441  1.4307 indirect
#> 69  1.8 1.5778 3.1081  1.5551 indirect
#> 70  1.9 1.5224 3.0698  1.6685 indirect
#> 71  2.0 1.4690 3.0293  1.7716 indirect
#> 72  2.1 1.4175 2.9868  1.8649 indirect
#> 73  2.2 1.3678 2.9427  1.9488 indirect
#> 74  2.3 1.3198 2.8971  2.0240 indirect
#> 75  2.4 1.2736 2.8502  2.0908 indirect
plot(traj)



```

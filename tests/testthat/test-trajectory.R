## ---- test-trajectory
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
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
    traj <- Trajectory(
      mu0 = c(3, 3, -3),
      time = 25,
      phi = phi,
      med = "m"
    )
    print(traj)
    summary(traj)
    plot(traj)
  },
  text = "test-trajectory"
)

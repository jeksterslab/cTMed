% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cTMed-indirect-central.R
\name{IndirectCentral}
\alias{IndirectCentral}
\title{Indirect Effect Centrality}
\usage{
IndirectCentral(phi, delta_t)
}
\arguments{
\item{phi}{Numeric matrix.
The drift matrix (\eqn{\boldsymbol{\Phi}}).
\code{phi} should have row and column names
pertaining to the variables in the system.}

\item{delta_t}{Vector of positive numbers.
Time interval
(\eqn{\Delta t}).}
}
\value{
Returns an object
of class \code{ctmedmed} which is a list with the following elements:
\describe{
\item{call}{Function call.}
\item{args}{Function arguments.}
\item{fun}{Function used ("IndirectCentral").}
\item{output}{A matrix of indirect effect centrality.}
}
}
\description{
Indirect Effect Centrality
}
\examples{
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
IndirectCentral(
  phi = phi,
  delta_t = 1
)

# Range of time intervals ---------------------------------------------------
indirect_central <- IndirectCentral(
  phi = phi,
  delta_t = 1:30
)
plot(indirect_central)

# Methods -------------------------------------------------------------------
# IndirectCentral has a number of methods including
# print, summary, and plot
indirect_central <- IndirectCentral(
  phi = phi,
  delta_t = 1:5
)
print(indirect_central)
summary(indirect_central)
plot(indirect_central)

}
\seealso{
Other Continuous Time Mediation Functions: 
\code{\link{DeltaBeta}()},
\code{\link{DeltaIndirectCentral}()},
\code{\link{DeltaMed}()},
\code{\link{DeltaTotalCentral}()},
\code{\link{Direct}()},
\code{\link{Indirect}()},
\code{\link{MCBeta}()},
\code{\link{MCIndirectCentral}()},
\code{\link{MCMed}()},
\code{\link{MCPhi}()},
\code{\link{MCTotalCentral}()},
\code{\link{Med}()},
\code{\link{PosteriorBeta}()},
\code{\link{PosteriorIndirectCentral}()},
\code{\link{PosteriorMed}()},
\code{\link{PosteriorPhi}()},
\code{\link{PosteriorTotalCentral}()},
\code{\link{Total}()},
\code{\link{TotalCentral}()},
\code{\link{Trajectory}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Continuous Time Mediation Functions}
\keyword{cTMed}
\keyword{network}

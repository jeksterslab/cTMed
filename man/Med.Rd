% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cTMed-med.R
\name{Med}
\alias{Med}
\title{Total, Direct, and Indirect Effects of X on Y
Through M
Over a Specific Time Interval
or a Range of Time Intervals}
\usage{
Med(phi, delta_t, from, to, med)
}
\arguments{
\item{phi}{Numeric matrix.
The drift matrix (\eqn{\boldsymbol{\Phi}}).
\code{phi} should have row and column names
pertaining to the variables in the system.}

\item{delta_t}{Vector of positive numbers.
Time interval
(\eqn{\Delta t}).}

\item{from}{Character string.
Name of the independent variable \eqn{X} in \code{phi}.}

\item{to}{Character string.
Name of the dependent variable \eqn{Y} in \code{phi}.}

\item{med}{Character vector.
Name/s of the mediator variable/s in \code{phi}.}
}
\value{
Returns an object
of class \code{ctmedmed} which is a list with the following elements:
\describe{
\item{call}{Function call.}
\item{args}{Function arguments.}
\item{fun}{Function used ("Med").}
\item{output}{A matrix of total, direct, and indirect effects.}
}
}
\description{
This function computes the total, direct, and indirect effects
of the independent variable \eqn{X}
on the dependent variable \eqn{Y}
through mediator variables \eqn{\mathbf{m}}
over a specific time interval \eqn{\Delta t}
or a range of time intervals
using the first-order stochastic differential equation model's
drift matrix \eqn{\boldsymbol{\Phi}}.
}
\details{
See \code{\link[=Total]{Total()}},
\code{\link[=Direct]{Direct()}}, and
\code{\link[=Indirect]{Indirect()}} for more details.
\subsection{Linear Stochastic Differential Equation Model}{

The measurement model is given by
\deqn{
    \mathbf{y}_{i, t}
    =
    \boldsymbol{\nu}
    +
    \boldsymbol{\Lambda}
    \boldsymbol{\eta}_{i, t}
    +
    \boldsymbol{\varepsilon}_{i, t},
    \quad
    \mathrm{with}
    \quad
    \boldsymbol{\varepsilon}_{i, t}
    \sim
    \mathcal{N}
    \left(
    \mathbf{0},
    \boldsymbol{\Theta}
    \right)
  }
where
\eqn{\mathbf{y}_{i, t}},
\eqn{\boldsymbol{\eta}_{i, t}},
and
\eqn{\boldsymbol{\varepsilon}_{i, t}}
are random variables
and
\eqn{\boldsymbol{\nu}},
\eqn{\boldsymbol{\Lambda}},
and
\eqn{\boldsymbol{\Theta}}
are model parameters.
\eqn{\mathbf{y}_{i, t}}
represents a vector of observed random variables,
\eqn{\boldsymbol{\eta}_{i, t}}
a vector of latent random variables,
and
\eqn{\boldsymbol{\varepsilon}_{i, t}}
a vector of random measurement errors,
at time \eqn{t} and individual \eqn{i}.
\eqn{\boldsymbol{\nu}}
denotes a vector of intercepts,
\eqn{\boldsymbol{\Lambda}}
a matrix of factor loadings,
and
\eqn{\boldsymbol{\Theta}}
the covariance matrix of
\eqn{\boldsymbol{\varepsilon}}.

An alternative representation of the measurement error
is given by
\deqn{
    \boldsymbol{\varepsilon}_{i, t}
    =
    \boldsymbol{\Theta}^{\frac{1}{2}}
    \mathbf{z}_{i, t},
    \quad
    \mathrm{with}
    \quad
    \mathbf{z}_{i, t}
    \sim
    \mathcal{N}
    \left(
    \mathbf{0},
    \mathbf{I}
    \right)
  }
where
\eqn{\mathbf{z}_{i, t}} is a vector of
independent standard normal random variables and
\eqn{
    \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right)
    \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right)^{\prime}
    =
    \boldsymbol{\Theta} .
  }

The dynamic structure is given by
\deqn{
    \mathrm{d} \boldsymbol{\eta}_{i, t}
    =
    \left(
    \boldsymbol{\iota}
    +
    \boldsymbol{\Phi}
    \boldsymbol{\eta}_{i, t}
    \right)
    \mathrm{d}t
    +
    \boldsymbol{\Sigma}^{\frac{1}{2}}
    \mathrm{d}
    \mathbf{W}_{i, t}
  }
where
\eqn{\boldsymbol{\iota}}
is a term which is unobserved and constant over time,
\eqn{\boldsymbol{\Phi}}
is the drift matrix
which represents the rate of change of the solution
in the absence of any random fluctuations,
\eqn{\boldsymbol{\Sigma}}
is the matrix of volatility
or randomness in the process, and
\eqn{\mathrm{d}\boldsymbol{W}}
is a Wiener process or Brownian motion,
which represents random fluctuations.
}
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
Med(
  phi = phi,
  delta_t = 1,
  from = "x",
  to = "y",
  med = "m"
)

# Range of time intervals ---------------------------------------------------
med <- Med(
  phi = phi,
  delta_t = 1:30,
  from = "x",
  to = "y",
  med = "m"
)
plot(med)

# Methods -------------------------------------------------------------------
# Med has a number of methods including
# print, summary, and plot
med <- Med(
  phi = phi,
  delta_t = 1:5,
  from = "x",
  to = "y",
  med = "m"
)
print(med)
summary(med)
plot(med)

}
\references{
Bollen, K. A. (1987).
Total, direct, and indirect effects in structural equation models.
Sociological Methodology, 17, 37.
\doi{10.2307/271028}

Deboeck, P. R., & Preacher, K. J. (2015).
No need to be discrete:
A method for continuous time mediation analysis.
Structural Equation Modeling: A Multidisciplinary Journal, 23 (1), 61–75.
\doi{10.1080/10705511.2014.973960}

Ryan, O., & Hamaker, E. L. (2021).
Time to intervene:
A continuous-time approach to network analysis and centrality.
Psychometrika, 87 (1), 214–252.
\doi{10.1007/s11336-021-09767-0}
}
\seealso{
Other Continuous Time Mediation Functions: 
\code{\link{DeltaBeta}()},
\code{\link{DeltaIndirectCentral}()},
\code{\link{DeltaMed}()},
\code{\link{DeltaTotalCentral}()},
\code{\link{Direct}()},
\code{\link{Indirect}()},
\code{\link{IndirectCentral}()},
\code{\link{MCBeta}()},
\code{\link{MCIndirectCentral}()},
\code{\link{MCMed}()},
\code{\link{MCPhi}()},
\code{\link{MCTotalCentral}()},
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
\keyword{effects}
\keyword{path}

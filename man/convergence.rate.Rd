\name{Numerical estimation of the convergence rate of Rosenthal's fail-safe number Nr}
\alias{convergence.rate}
\title{
Numerical estimation of the convergence rate of Rosenthal's fail-safe number Nr
}

\description{
Numerical estimation of the convergence rate of Rosenthal's fail-safe number Nr.
}

\usage{
convergence.rate(k = seq( 10, 5000, by = 10), R = 1000, alpha = 0.05)
}

\arguments{
\item{k}{
A grid of number of studies to consider.
}
\item{R}{
The number of repeats for each number of studies.
}
\item{alpha}{
The significance level, set to 0.05 by default.
}
}

\details{
This function replicates the Figures 6 and 5 in Fragkos, Tsagris and
Frangos (2017).
}

\value{
Two plots, the absolute relative error of Nr agains the number of studies and
the logarithm of absolute relative error against the logarithm of the number of
studies and the coefficients of the regression model of the second plot.
The second coefficient is the numerically estimated convergence rate of Nr.
}

\references{
Konstantinos C. Fragkos, Michail Tsagris and Christos C. Frangos (2017).
Exploring the distribution for the estimator of Rosenthal's "fail-safe"
number of unpublished studies in meta-analysis. Communications in
Statistics-Theory and Methods, 46(11):5672--5684.

}

\author{
Michail Tsagris and Constantinos Frangos

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}
and Constantinos Frangos \email{kfragkos@outlook.com}.
}

\seealso{
\code{ \link{den.plot}, \link{truncnorm.nr.density}, \link{rosenthal} }
}

\examples{
convergence.rate(k = seq( 50, 500, by = 50), R = 1000, alpha = 0.05)
}



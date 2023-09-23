\name{Plot of both densities of Nr}
\alias{den.plot}
\title{
Plot of both densities of Nr
}

\description{
Plot of both densities of Nr.
}

\usage{
den.plot(k, max_k = 20 * k, dist = "truncnorm")
}

\arguments{
\item{k}{
The number of studies.
}
\item{max_k}{
The maximum number for which the densities are calcualted. It is set to
\eqn{20k} by default.
}
\item{dist}{
The distribution to plot, either "truncnorm", "foldnorm" or "both".
}
}

\details{
The function plot the density of Nr assuming  a truncated normal
(Equation (9)) or a folded normal (Equation (15)) in Fragkos,
Tsagris and Frangos (2017).
}

\value{
The density plot of Nr assuming either a truncated normal or a folded normal.
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
\code{ \link{truncnorm.nr.density}, \link{rosenthal} }
}

\examples{
den.plot(30, dist = "both")
}



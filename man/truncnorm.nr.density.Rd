\name{Density of Nr assuming a truncated normal or a folded normal}
\alias{truncnorm.nr.density}
\alias{foldnorm.nr.density}
\title{
Density of Nr assuming a truncated normal or a folded normal
}

\description{
Density of Nr assuming a truncated normal or a folded normal.
}

\usage{
truncnorm.nr.density(nr, k, alpha = 0.05)
foldnorm.nr.density(nr, k, alpha = 0.05)
}

\arguments{
\item{nr}{
The value of Nr, which must be positive aparently.
}
\item{k}{
The number of studies.
}
\item{alpha}{
The significance level, set to 0.05 by default.
}
}

\details{
The function calculates the density of Nr assuming either a truncated normal
(Equation (9)) or a folded normal (Equation (15)) in Fragkos,
Tsagris & Frangos (2017).
}

\value{
The density value of Nr assuming either a truncated normal or a folded normal.
}

\references{
Konstantinos C. Fragkos, Michail Tsagris and Christos C. Frangos (2017).
Exploring the distribution for the estimator of Rosenthal's "fail-safe"
number of unpublished studies in meta-analysis.
Communications in Statistics-Theory and Methods, 46(11):5672--5684.
}

\author{
Michail Tsagris, Constantinos Frangos, and Christos Frangos.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr},
Constantinos Frangos \email{kfragkos@outlook.com} and Christos Frangos
\email{cfragos@teiath.gr}.
}

\seealso{
\code{ \link{den.plot}, \link{rosenthal}, \link{convergence.rate} }
}

\examples{
truncnorm.nr.density(100, k = 30)
foldnorm.nr.density(100, k = 30)
}



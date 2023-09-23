\name{Rosenthal's fail-safe number Nr}
\alias{rosenthal}
\title{
Rosenthal's fail-safe number Nr
}

\description{
Rosenthal's fail-safe number Nr.
}

\usage{
rosenthal(stat, se, alpha = 0.05)
}

\arguments{
\item{stat}{
A vector with the statistics (be careful: not the test statistics).
}
\item{se}{
A vector with the standard errors of the stat.
}
\item{alpha}{
The significance level, set to 0.05 by default.
}
}

\details{
The function Calculates of Rosenthal's fail-safe number Nr.
}

\value{
A vector with two values, Rosenthal's fail-safe number Nr and the rule of
thumb, \eqn{5k + 10}, where \eqn{k} denotes the number of studies.
}

\references{
Konstantinos C. Fragkos, Michail Tsagris and Christos C. Frangos (2017).
Exploring the distribution for the estimator of Rosenthal's "fail-safe"
number of unpublished studies in meta-analysis. Communications in
Statistics-Theory and Methods, 46(11):5672--5684.

Rosenthal R. (1979). The file drawer problem and tolerance for null results.
Psychological Bulletin, 86, 638--641.
}

\author{
Michail Tsagris and Constantinos Frangos

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}
and Constantinos Frangos \email{kfragkos@outlook.com}.
}

\seealso{
\code{ \link{truncnorm.nr.density}, \link{den.plot}, \link{convergence.rate}
}
}

\examples{
stat <- rnorm(30, 3, 0.2)
se <- rchisq(30, 1)
rosenthal(stat, se)
}



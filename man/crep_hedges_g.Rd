% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crep_hedges_g.R
\name{crep_hedges_g}
\alias{crep_hedges_g}
\title{crep_hedges_g}
\usage{
crep_hedges_g(m1, sd1, n1, m2, sd2, n2, M = 0, icc = 0)
}
\arguments{
\item{m1}{The mean of group 1.}

\item{sd1}{Standard deviation of group 1.}

\item{n1}{N of group 1.}

\item{m2}{Mean of group 2.}

\item{sd2}{Standard deviation of group 2.}

\item{n2}{N of group 2.}

\item{M}{Number of groups (optional, for clustered data)}

\item{icc}{The intraclass correlation coefficient (optional,
for clustered data)}
}
\value{
g
}
\description{
Calculates Hedges' g from summary statistics.
}
\examples{

CREP::crep_hedges_g(15, 4, 20, 40, 4, 32)

}

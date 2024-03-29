% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lower_level_disprop_analysis.R
\name{ic}
\alias{ic}
\title{Information component}
\usage{
ic(obs = NULL, exp = NULL, shrinkage = 0.5, conf_lvl = 0.95)
}
\arguments{
\item{obs}{A numeric vector with observed counts, i.e. number of reports
for the selected drug-event-combination. Note that shrinkage (e.g. +0.5) is added
inside the function and should not be included here.}

\item{exp}{A numeric vector with expected counts, i.e. number of reports
to be expected given a comparator or \emph{background}. Note that shrinkage
(e.g. +0.5) is added inside the function and should not be included here.}

\item{shrinkage}{A non-negative numeric value, to be added to
observed and expected count. Default is 0.5.}

\item{conf_lvl}{Confidence level of confidence or credibility intervals.
Default is 0.95 (i.e. 95 \% confidence interval).}
}
\value{
A tibble with three columns (point estimate and credibility bounds).
}
\description{
Calculates the information component ("IC") and credibility
interval, used in disproportionality analysis.
}
\details{
The IC is a log2-transformed observed-to-expected ratio, based on
the relative reporting rate (RRR) for counts, but modified with an addition
of "shrinkage" to protect against spurious associations.

\deqn{\hat{IC} = log_{2}(\frac{\hat{O}+k}{\hat{E}+k})}

where \eqn{\hat{O}} = observed number of reports, \eqn{k} is the shrinkage
(typically +0.5), and expected \eqn{\hat{E}} is (for RRR, and using the
entire database as comparator or \emph{background}) estimated as

\deqn{ \hat{E} = \frac{\hat{N}_{drug} \times \hat{N}_{event}}{\hat{N}_{TOT}}}

where \eqn{\hat{N}_{drug}}, \eqn{\hat{N}_{event}} and \eqn{\hat{N}_{TOT}} are the number of
reports with the drug, the event, and in the whole database respectively.

The credibility interval is created from the quantiles of the posterior
gamma distribution with shape (\eqn{\hat{S}}) and rate (\eqn{\hat{R}}) parameters as

\deqn{\hat{S} = \hat{O} + k}
\deqn{\hat{R} = \hat{E} + k}

using the \code{stats::qgamma} function. Parameter \eqn{k} is the shrinkage defined
earlier. For completeness, a credibility interval of the gamma distributed \eqn{X} (i.e.
\eqn{X \sim \Gamma(\hat{S}, \hat{R})} where \eqn{\hat{S}} and \eqn{\hat{R}} are shape and rate parameters)
with associated quantile function \eqn{Q_X(p)} for a significance level \eqn{\alpha} is
constructed as

\deqn{[Q_X(\alpha/2), Q_X(1-\alpha/2)]}
}
\section{Further details}{

From a bayesian point-of-view, the credibility interval of the IC is constructed
from the poisson-gamma conjugacy. The shrinkage constitutes a prior of
observed and expected of 0.5. A shrinkage of +0.5 with a gamma-quantile based 95 \%
credibility interval cannot have lower bound above 0 unless the observed count
exceeds 3. One benefit of \eqn{log_{2}} is to provide
a log-scale for convenient plotting of multiple IC values side-by-side.
}

\examples{
ic(obs = 20, exp = 10)
# Note that obs and exp can be vectors (of equal length, no recycling allowed)
ic(obs = c(20, 30), exp = c(10, 10))
}
\references{
\insertRef{Nor_n_2011}{pvda}
}

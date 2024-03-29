% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConnectednessFrequency.R
\name{FrequencyConnectedness}
\alias{FrequencyConnectedness}
\title{Baruník and Křehlík (2018) frequency connectedness approach}
\usage{
FrequencyConnectedness(
  Phi,
  Sigma,
  nfore,
  partition = c(pi, pi/2, 0),
  generalized = TRUE,
  orth = FALSE,
  no.corr = FALSE,
  scenario = "ABS"
)
}
\arguments{
\item{Phi}{VAR coefficient matrix}

\item{Sigma}{Residual variance-covariance matrix}

\item{nfore}{H-step ahead forecast horizon}

\item{partition}{Frequency spectrum}

\item{generalized}{Orthorgonalized/generalized FEVD}

\item{orth}{Orthorgonalized shocks}

\item{no.corr}{Uncorrelated shocks}

\item{scenario}{ABS or WTH}
}
\value{
Get connectedness measures
}
\description{
This function calculates the Baruník and Křehlík (2018) frequency connectedness measures.
}
\examples{
# Replication of frequencyConnectedness (https://github.com/tomaskrehlik/frequencyConnectedness)
#data("exampleSim")
#x = zoo(exampleSim[1:600,], order.by=1:600)
#partition = c(pi+0.00001, pi/4, 0)
#dca = ConnectednessApproach(x, model="VAR",
#                            connectedness="Frequency",
#                            nlag=2, nfore=100,
#                            Connectedness_config=list(
#                            FrequencyConnectedness=list(partition=partition, generalized=TRUE)))
#dca$TABLE
}
\references{
Baruník, J., & Křehlík, T. (2018). Measuring the frequency dynamics of financial connectedness and systemic risk. Journal of Financial Econometrics, 16(2), 271-296.
}
\author{
David Gabauer
}

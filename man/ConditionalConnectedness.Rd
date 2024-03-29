% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConnectednessConditional.R
\name{ConditionalConnectedness}
\alias{ConditionalConnectedness}
\title{ConditionalConnectedness}
\usage{
ConditionalConnectedness(dca, group = c(1, 2, 3))
}
\arguments{
\item{dca}{Dynamic connectedness object}

\item{group}{Group vector}
}
\value{
Get connectedness measures
}
\description{
This function computes the conditional connectedness table
}
\examples{
#Replication of Chatzianzoniou, Gabauer and Stenfors (2022)
#data(cgs2022)
#cgs2022 = cgs2022[1:300,]
#dca = ConnectednessApproach(cgs2022, model="TVP-VAR",
#                              connectedness="Time",
#                             nlag=1, nfore=10, window.size=250,
#                             VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99,
#                              prior="BayesPrior")))
#cc = ConditionalConnectedness(dca, group=c(1,4,7,10,13,16))
#cc$TABLE
}
\references{
Chatziantoniou, I., Gabauer, D., & Stenfors, A. (2021). Independent Policy, Dependent Outcomes: A Game of Cross-Country Dominoes across European Yield Curves (No. 2021-06). University of Portsmouth, Portsmouth Business School, Economics and Finance Subject Group.
}
\author{
David Gabauer
}

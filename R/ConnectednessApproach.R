
#' @title Connectedness Approach
#' @description This function provides a modular framework combining various models and connectedness frameworks.
#' @param x zoo data matrix
#' @param nlag Lag length
#' @param nfore H-step ahead forecast horizon
#' @param window.size Rolling-window size or Bayes Prior size
#' @param model Estimation model
#' @param connectedness Type of connectedness approach
#' @param VAR_config Config for estimation model
#' @param Connectedness_config Config for connectedness approach
#' @return Get connectedness measures
#' @examples
#' #data(dy2009)
#' #dca = ConnectednessApproach(dy2009, model="VAR",
#' #                            connectedness="Time",
#' #                            nlag=2, nfore=10,
#' #                            Connectedness_config=list(TimeConnectedness=list(generalized=FALSE)))
#' #dca$TABLE
#' #data(dy2012)
#' #dca = ConnectednessApproach(dy2012, model="VAR",
#' #                            connectedness="Time",
#' #                            nlag=2, nfore=10,
#' #                            Connectedness_config=list(TimeConnectedness=list(generalized=FALSE)))
#' #dca$TABLE
#' #data("lw2021")
#' #dca = ConnectednessApproach(lw2021, model="VAR",
#' #                            connectedness="Joint",
#' #                            nlag=2, nfore=30)
#' #dca$TABLE
#' #Replication of Balcilar et al. (2021)
#' #data("bgu2021")
#' #dca = ConnectednessApproach(bgu2021, model="TVP-VAR",
#' #                            connectedness="Extended Joint",
#' #                            nlag=1, nfore=20,
#' #                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99,
#' #                                            prior="MinnesotaPrior", gamma=0.1)))
#' #dca$TABLE
#' #data("acg2020")
#' #dca = ConnectednessApproach(acg2020, model="TVP-VAR",
#' #                            connectedness="Time",
#' #                            nlag=1, nfore=12,
#' #                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96,
#' #                                            prior="MinnesotaPrior", gamma=0.1)))
#' #dca$TABLE
#' # Replication of frequencyConnectedness (https://github.com/tomaskrehlik/frequencyConnectedness)
#' #data("exampleSim")
#' #x = zoo(exampleSim[1:600,], order.by=1:600)
#' #partition = c(pi+0.00001, pi/4, 0)
#' #dca = ConnectednessApproach(x, model="VAR",
#' #                            connectedness="Frequency",
#' #                            nlag=2, nfore=100,
#'
#' #fit = VAR(x, configuration=list(nlag=2))
#' #dca = FrequencyConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=100, partition=partition)
#' #dca$TABLE
#' @import progress
#' @references
#' Diebold, F. X., & Yilmaz, K. (2009). Measuring financial asset return and volatility spillovers, with application to global equity markets. The Economic Journal, 119(534), 158-171.\\
#' Diebold, F. X., & Yilmaz, K. (2012). Better to give than to receive: Predictive directional measurement of volatility spillovers. International Journal of Forecasting, 28(1), 57-66.\\
#' Baruník, J., & Křehlík, T. (2018). Measuring the frequency dynamics of financial connectedness and systemic risk. Journal of Financial Econometrics, 16(2), 271-296.\\
#' Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. Journal of Risk and Financial Management, 13(4), 84.\\
#' Lastrapes, W. D., & Wiesen, T. F. (2021). The joint spillover index. Economic Modelling, 94, 681-691.\\
#' Balcilar, M., Gabauer, D., & Umar, Z. (2021). Crude Oil futures contracts and commodity markets: New evidence from a TVP-VAR extended joint connectedness approach. Resources Policy, 73, 102219.\\
#' Chatziantoniou, I., & Gabauer, D. (2021). EMU risk-synchronisation and financial fragility through the prism of dynamic connectedness. The Quarterly Review of Economics and Finance, 79, 1-14.\\
#' Gabauer, D. (2021). Dynamic measures of asymmetric & pairwise connectedness within an optimal currency area: Evidence from the ERM I system. Journal of Multinational Financial Management, 60, 100680.
#' @author David Gabauer
#' @export
ConnectednessApproach = function(x,
                                 nlag=1, nfore=10, window.size=NULL,
                                 model=c("VAR", "QVAR", "LASSO", "Ridge", "Elastic", "TVP-VAR"),
                                 connectedness=c("Time","Frequency", "Joint", "Extended Joint"),
                                 VAR_config=list(
                                   QVAR=list(tau=0.5),
                                   ElasticNet=list(nfolds=10, alpha=NULL, loss="mae", n_alpha=10),
                                   TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="BayesPrior", gamma=0.01)),
                                 Connectedness_config = list(
                                   TimeConnectedness=list(generalized=TRUE),
                                   FrequencyConnectedness=list(partition=c(pi,pi/2,0), generalized=TRUE, scenario="ABS")
                                 )
) {
  if (class(x)!="zoo") {
    stop("Data needs to be of type 'zoo'")
  }
  if (length(model)>1) {
    model = "VAR"
  }
  if (length(connectedness)>1) {
    connectedness = "Time"
  }
  NAMES = colnames(x)
  k = ncol(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }

  t = nrow(x)
  if (is.null(window.size)) {
    window.size = nrow(x)
    t0 = 1
  } else {
    window.size = window.size - nlag
    t0 = t - window.size + 1
  }

  if (model=="VAR") {
    var_model = VAR
    configuration = list(nlag=nlag)
  } else if (model=="QVAR") {
    var_model = QVAR
    configuration = list(nlag=nlag, tau=VAR_config$QVAR$tau)
  } else if (model=="LASSO") {
    var_model = ElasticNetVAR
    configuration = list(nlag=nlag, alpha=1, nfolds=VAR_config$ElasticNet$nfolds, loss=VAR_config$ElasticNet$loss)
  } else if (model=="Ridge") {
    var_model = ElasticNetVAR
    configuration = list(nlag=nlag, alpha=0, nfolds=VAR_config$ElasticNet$nfolds, loss=VAR_config$ElasticNet$loss)
  } else if (model=="Elastic") {
    var_model = ElasticNetVAR
    configuration = list(nlag=nlag, alpha=VAR_config$ElasticNet$alpha, nfolds=VAR_config$ElasticNet$nfolds,
                         loss=VAR_config$ElasticNet$loss, n_alpha=VAR_config$ElasticNet$n_alpha)
  } else if (model=="TVP-VAR") {
    prior_ = VAR_config$TVPVAR$prior
    if (prior_=="MinnesotaPrior") {
      prior = MinnesotaPrior(gamma=VAR_config$TVPVAR$gamma, k=k, nlag=nlag)
    } else if (prior_=="UninformativePrior") {
      prior = UninformativePrior(k=k, nlag=nlag)
    } else if (prior_=="BayesPrior") {
      prior = BayesPrior(x=x[1:window.size,], nlag=nlag)
    } else {
      stop("Error Prior choice")
    }
    var_model = TVPVAR
    configuration = list(nlag=nlag, l=c(VAR_config$TVPVAR$kappa1, VAR_config$TVPVAR$kappa2), prior=prior)
  } else {
    stop("Model does not exist")
  }

  print("Estimating model")
  if (model=="TVP-VAR") {
    fit = var_model(x, configuration=configuration)
    B_t = fit$B_t
    Q_t = fit$Q_t
  } else {
    Q_t = array(NA, c(k, k, t0))
    B_t = array(NA, c(k, k*nlag, t0))
    pb = progress_bar$new(total=t0)
    for (i in 1:t0) {
      fit = var_model(x[i:(i+window.size-1),], configuration=configuration)
      B_t[,,i] = fit$B
      Q_t[,,i] = fit$Q
      pb$tick()
    }
  }
  DATE = as.character(zoo::index(x))
  date = DATE[(length(DATE)-dim(Q_t)[3]+1):length(DATE)]
  dimnames(Q_t)[[1]] = dimnames(Q_t)[[2]] = NAMES
  dimnames(Q_t)[[3]] = as.character(date)

  print("Computing connectedness measures")
  if (connectedness=="Time") {
    dca = DynamicConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore,
                               generalized=Connectedness_config$TimeConnectedness$generalized)
  } else if (connectedness=="Frequency") {
    dca = FrequencyConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore,
                                 partition=Connectedness_config$FrequencyConnectedness$partition,
                                 generalized=Connectedness_config$FrequencyConnectedness$generalized,
                                 scenario=Connectedness_config$FrequencyConnectedness$scenario)
  } else if (connectedness=="Joint") {
    dca = JointConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore)
  } else if (connectedness=="Extended Joint") {
    dca = ExtendedJointConnectedness(Phi=B_t, Sigma=Q_t, nfore=nfore)
  } else {
    stop("Connectedness approach does not exist")
  }
  dca
}


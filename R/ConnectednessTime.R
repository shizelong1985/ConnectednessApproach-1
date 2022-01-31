
#' @title Diebold and Yilmaz (2009, 2012) connectedness approach
#' @description This function allows to calculate the Diebold and Yilmaz (2009, 2012) connectedness measures.
#' @param Phi VAR coefficient matrix
#' @param Sigma Residual variance-covariance matrix
#' @param nfore H-step ahead forecast horizon
#' @param generalized Orthorgonalized/generalized FEVD
#' @return Get connectedness measures
#' @examples
#' #Replication of DY2009
#' #data("dy2009")
#' #fit = VAR(dy2009, configuration=list(nlag=2))
#' #dca = DynamicConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=10, generalized=FALSE)
#' #dca$TABLE
#'
#' #Replication of DY2012
#' #data("dy2012")
#' #fit = VAR(dy2012, configuration=list(nlag=4))
#' #dca = DynamicConnectedness(Phi=fit$B, Sigma=fit$Q, nfore=10, generalized=TRUE)
#' #dca$TABLE
#'
#' #data("acg2020")
#' #prior = MinnesotaPrior(0.5, k=ncol(acg2020), nlag=1)
#' #fit = TVPVAR(acg2020, configuration=list(l=c(0.99,0.99), nlag=1, prior))
#' #dca = DynamicConnectedness(Phi=fit$B_t, Sigma=fit$Q_t, nfore=10, generalized=TRUE)
#' #dca$TABLE
#' @references
#' Diebold, F. X., & Yilmaz, K. (2009). Measuring financial asset return and volatility spillovers, with application to global equity markets. The Economic Journal, 119(534), 158-171.\\
#' Diebold, F. X., & Yilmaz, K. (2012). Better to give than to receive: Predictive directional measurement of volatility spillovers. International Journal of Forecasting, 28(1), 57-66.
#' @author David Gabauer
#' @export
DynamicConnectedness = function(Phi, Sigma, nfore=10, generalized=TRUE) {
  NAMES = colnames(Sigma)
  k = ncol(Sigma)
  if (length(dim(Phi))==2) {
    Phi = array(Phi, c(nrow(Phi),ncol(Phi),1))
  }
  if (length(dim(Sigma))==2) {
    Sigma = array(Sigma, c(nrow(Sigma),ncol(Sigma),1))
  }
  t = dim(Sigma)[3]

  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  date = dimnames(Sigma)[[3]]
  TCI = cTCI = array(NA, c(t,1), dimnames=list(as.character(date), "TCI"))
  NPDC = NET = FROM = TO = array(NA, c(t, k), dimnames=list(as.character(date), NAMES))
  CT = PCI = NPSO = INFLUENCE = array(NA, c(k, k, t), dimnames=list(NAMES, NAMES, as.character(date)))
  for (i in 1:t) {
    CT[,,i] = FEVD(Phi=Phi[,,i], Sigma=Sigma[,,i], nfore=nfore, generalized=generalized, type="time")$FEVD
  }
  pb = progress::progress_bar$new(total=t)
  for (i in 1:t) {
    ct = ConnectednessTable(CT[,,i])
    TCI[i,] = ct$TCI
    cTCI[i,] = ct$cTCI
    TO[i,] = ct$TO
    FROM[i,] = ct$FROM
    NET[i,] = ct$NET
    NPDC[i,] = ct$NPDC
    NPSO[,,i] = ct$NPSO
    INFLUENCE[,,i] = ct$INFLUENCE
    PCI[,,i] = ct$PCI
    pb$tick()
  }
  TABLE = ConnectednessTable(CT)$TABLE
  return = list(TABLE=TABLE, FEVD=CT, TCI=TCI, cTCI=cTCI, TO=TO, FROM=FROM,
                NET=NET, NPDC=NPDC, NPSO=NPSO, PCI=PCI, INFLUENCE=INFLUENCE)
}

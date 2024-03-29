
#' @title Connectedness table
#' @description This function provides standard connectedness table.
#' @param FEVD Forecast error variance decomposition
#' @param digit Number of decimal places
#' @return Get connectedness table
#' @examples
#' #data(dy2012)
#' #fit = VAR(dy2012, configuration=list(nlag=1))
#' #fevd = FEVD(Phi=fit$B, Sigma=fit$Q, nfore=10, type="time", generalized=TRUE)$FEVD
#' #dca = ConnectednessTable(fevd)
#' @references
#' Chatziantoniou, I., & Gabauer, D. (2021). EMU risk-synchronisation and financial fragility through the prism of dynamic connectedness. The Quarterly Review of Economics and Finance, 79, 1-14.\\
#' Gabauer, D. (2021). Dynamic measures of asymmetric & pairwise connectedness within an optimal currency area: Evidence from the ERM I system. Journal of Multinational Financial Management, 60, 100680.
#' @export
ConnectednessTable = function(FEVD, digit=2) {
  NAMES = colnames(FEVD)
  k = dim(FEVD)[1]
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  CT = apply(FEVD,1:2,mean)*100 # spillover from others to one specific
  OWN = diag(diag(CT))
  TO = colSums(CT-OWN)
  FROM = rowSums(CT-OWN)
  NET = TO-FROM
  TCI = mean(TO)
  cTCI = TCI*k/(k-1)
  NPSO = CT-t(CT)
  NPDC = rowSums(NPSO<0)
  INFLUENCE = 100*abs(NPSO/t(t(CT)+CT))
  table = format(round(cbind(CT,FROM),digit),nsmall=digit)
  to = c(format(round(c(TO,sum(TO)),digit),nsmall=digit))
  inc = c(format(round(colSums(CT), digit),nsmall=digit), "cTCI/TCI")
  tci = paste0(format(round(cTCI,digit),nsmall=digit),"/",format(round(TCI,digit),nsmall=digit))
  net = c(format(round(NET,digit),nsmall=digit))
  net = c(net, tci) 
  npdc = c(format(round(NPDC,digit),nsmall=digit), "")
  
  TABLE = rbind(table,to,inc,net,npdc)
  colnames(TABLE) = c(NAMES,"FROM")
  rownames(TABLE) = c(NAMES,"TO","Inc.Own","NET","NPDC")
  PCI = matrix(NA, k, k)
  for (i in 1:k) {
    for (j in 1:k) {
      PCI[i,j] = 200*(CT[i,j]+CT[j,i])/(CT[i,i]+CT[i,j]+CT[j,i]+CT[j,j])
    }
  }
  return = list(FEVD=CT, TCI=TCI, cTCI=cTCI, PCI=PCI,
                TO=TO, FROM=FROM, NET=NET, NPSO=NPSO, TABLE=TABLE,
                NPDC=NPDC, INFLUENCE=INFLUENCE)
}

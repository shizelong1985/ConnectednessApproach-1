
#' @title Partial Connectedness Measures
#' @description This function results in partial connectedness measures
#' @param dca Dynamic connectedness object
#' @param group Vector of group indices
#' @param corrected Should corrected TCI or normal TCI be used
#' @return Get connectedness measures
#' @examples
#' #Replication of Gabauer and Gupta (2018)
#' #data("gg2018")
#' #dca = ConnectednessApproach(gg2018, model="TVP-VAR",
#' #                             connectedness="Time",
#' #                            nlag=1, nfore=10, window.size=200,
#' #                             VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99,
#' #                                             prior="BayesPrior")))
#' #cd = ConnectednessAggregate(dca, groups=list("US"=c(1,2,3,4), "JP"=c(5,6,7,8)))
#' #plot(cd$NPSO_group[1,2,],type="l", ylim=c(-3,6), las=1, xaxs="i", xlab="", ylab="")
#' #lines(cd$NPSO_group[2,1,], col=2)
#' #lines(cd$NPSO_group[2,1,]-cd$NPSO_group[1,2,], col=3)
#' #abline(h=0, lty=3)
#' @references Gabauer, D., & Gupta, R. (2018). On the transmission mechanism of country-specific and international economic uncertainty spillovers: Evidence from a TVP-VAR connectedness decomposition approach. Economics Letters, 171, 63-71.
#' @author David Gabauer
#' @export
ConnectednessPartial = function(dca, group=c(1,2), corrected=TRUE) {
  ct = dca$FEVD
  NAMES = dimnames(ct)[[1]]
  date = dimnames(ct)[[3]]
  k = dim(ct)[1]
  t = dim(ct)[3]

  CT_ = ct
  CT = ct*0
  for (i in group) {
    CT[,i,] = ct[,i,]
    CT[i,,] = ct[i,,]
    CT_[,i,] = ct[,i,]*0
    CT_[i,,] = ct[i,,]*0
  }

  TCI = cTCI = array(NA, c(t,1), dimnames=list(as.character(date), "TCI"))
  NPDC = NET = FROM = TO = array(NA, c(t, k), dimnames=list(date, NAMES))
  for (i in 1:t) {
    dca = ConnectednessTable(CT[,,i])
    TCI[i,] = dca$TCI
    cTCI[i,] = dca$cTCI
    TO[i,] = dca$TO
    FROM[i,] = dca$FROM
    NET[i,] = dca$NET
  }

  TCI_ = cTCI_ = array(NA, c(t,1), dimnames=list(as.character(date), "TCI"))
  NPDCv = NET_ = FROM_ = TO_ = array(NA, c(t, k), dimnames=list(date, NAMES))
  for (i in 1:t) {
    dca = ConnectednessTable(CT_[,,i])
    TCIL_[i,] = dca$TCI
    cTCI_[i,] = dca$cTCI
    TO_[i,] = dca$TO
    FROM_[i,] = dca$FROM
    NET_[i,] = dca$NET
  }

  return = list(TCI=TCI, cTCI=cTCI, NET=NET, TO=TO, FROM=FROM,
                TCIL_=TCI_, cTCI_=cTCI_, NET_=NET_, TO_=TO_, FROM_=FROM_)
}

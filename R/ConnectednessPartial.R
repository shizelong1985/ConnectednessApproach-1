
#' @title Partial Connectedness Measures
#' @description This function results in partial connectedness measures
#' @param dca Dynamic connectedness object
#' @param group Vector of group indices
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
PartialConnectedness = function(dca, group=c(1,2)) {
  if (dca$approach=="Frequency" | dca$approach=="Joint") {
    stop(paste("Partial connectedness measures are not implemented for",dca$approach, "connectedness"))
  } else {
    ct = dca$CT
    NAMES = dimnames(ct)[[1]]
    date = dimnames(ct)[[3]]
    k = dim(ct)[1]
    t = dim(ct)[3]
  
    CTx = ct
    CT = ct*0
    for (i in group) {
      CT[,i,] = ct[,i,]
      CT[i,,] = ct[i,,]
      CTx[,i,] = ct[,i,]*0
      CTx[i,,] = ct[i,,]*0
    }
  
    TCI = array(NA, c(t,2,2), dimnames=list(as.character(date), c("Inc","Exc"), c("cTCI", "TCI")))
    NPDC = NET = FROM = TO = array(NA, c(t, k, 2), dimnames=list(date, NAMES,c("Inc","Exc")))
    for (i in 1:t) {
      dca = ConnectednessTable(CT[,,i])
      TCI[i,1,] = c(dca$cTCI, dca$TCI)
      TO[i,,1] = dca$TO
      FROM[i,,1] = dca$FROM
      NET[i,,1] = dca$NET
      NPDC[i,,1] = dca$NPDC
    }
    for (i in 1:t) {
      dca = ConnectednessTable(CTx[,,i])
      TCI[i,2,] = c(dca$cTCI, dca$TCI)
      TO[i,,2] = dca$TO
      FROM[i,,2] = dca$FROM
      NET[i,,2] = dca$NET
      NPDC[i,,2] = dca$NPDC
    }
    TABLE = ConnectednessTable(CT)$TABLE
    TABLEx = ConnectednessTable(CTx)$TABLE
    
    return = list(TABLE=list(TABLE,TABLEx), TCI=TCI, NET=NET, TO=TO, FROM=FROM, NPDC=NPDC, approach="partial")
  }
}

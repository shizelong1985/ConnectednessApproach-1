
#' @title Connectedness Decomposition
#' @description This function decomposes the connectedness tables
#' @param dca Dynamic connectedness object
#' @param groups List of at least two group vectors
#' @return Get connectedness measures
#' @examples
#' #Replication of Gabauer and Gupta (2018)
#' #data("gg2018")
#' #dca = ConnectednessApproach(gg2018, model="TVP-VAR",
#' #                             connectedness="Time",
#' #                            nlag=1, nfore=10, window.size=200,
#' #                             VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99,
#' #                                             prior="BayesPrior")))
#' #cd = ConnectednessDecomposition(dca, groups=list("US"=c(1,2,3,4), "JP"=c(5,6,7,8)))
#' #plot(cd$NPSO_group[1,2,],type="l", ylim=c(-3,6), las=1, xaxs="i", xlab="", ylab="")
#' #lines(cd$NPSO_group[2,1,], col=2)
#' #lines(cd$NPSO_group[2,1,]-cd$NPSO_group[1,2,], col=3)
#' #abline(h=0, lty=3)
#' @references Gabauer, D., & Gupta, R. (2018). On the transmission mechanism of country-specific and international economic uncertainty spillovers: Evidence from a TVP-VAR connectedness decomposition approach. Economics Letters, 171, 63-71.
#' @author David Gabauer
#' @export
ConnectednessDecompose = function(dca, groups=list(c(1), c(2:k))) {
  if (dca$approach=="Frequency" | dca$approach=="Joint") {
    stop(paste("Decomposed connectedness measures are not implemented for",dca$approach, "connectedness"))
  } else {
    ct = 100*dca$CT
    NAMES = colnames(ct)
    k = dim(ct)[2]
    if (length(dim(ct))==2) {
      ct = array(ct, c(k,k,1),dimnames=list(NAMES,NAMES))
    }
    ct_inter = ct_wo = ct
    date = as.character(dimnames(ct)[[3]])
    t = dim(ct)[3]

    m = length(groups)
    NAMES_group = names(groups)
    if (is.null(NAMES_group)) {
      NAMES_group = paste0("GROUP", 1:m)
    }
  
    for (i in 1:m) {
      for (j in 1:m) {
        if (i>j) {
          group_1 = groups[[i]]
          group_2 = groups[[j]]
          ct_wo[group_1,group_2,] = 0
          ct_wo[group_2,group_1,] = 0
          ct_inter[group_1,group_1,] = 0
          ct_inter[group_2,group_2,] = 0
        }
      }
    }
  
    TCI_wo = array(NA, c(t, 2, 2), dimnames=list(date, c("TCI","iTCI"), c("cTCI", "TCI")))
    PCI_wo = NPSO_wo = array(NA, c(k, k, t), dimnames=list(NAMES,NAMES,date))
    TO_wo = FROM_wo = NET_wo = array(NA, c(t, k), dimnames=list(date, NAMES))
    for (i in 1:t) {
      dca_ = ConnectednessTable(ct_wo[,,i]/100)
      TO_wo[i,] = dca_$TO
      FROM_wo[i,] = dca_$FROM
      NET_wo[i,] = dca_$NET
      NPSO_wo[,,i] = dca_$NPSO
      PCI_wo[,,i] = dca_$PCI
      TCI_wo[i,1,] = c(dca_$cTCI, dca_$TCI)
    }
    TCI_group = array(NA, c(t,m,2), dimnames=list(date, NAMES_group, c("cTCI","TCI")))
    for (i in 1:m) {
      group = groups[i][[1]]
      TCI_group[,i,1] = rowSums(TO_wo[,group,drop=FALSE])/(k-1)
      TCI_group[,i,2] = rowSums(TO_wo[,group,drop=FALSE])/k
    }
    TCI_wo[,2,1] = apply(ct_inter,3,sum)/(k-1)
    TCI_wo[,2,2] = apply(ct_inter,3,sum)/k
    TABLE = ConnectednessTable(ct_wo/100)$TABLE
    return = list(TABLE=TABLE, gTCI=TCI_group, TCI=TCI_wo, TO=TO_wo, FROM=FROM_wo, 
                  NET=NET_wo, NPSO=NPSO_wo, PCI=PCI_wo, approach="Decompose")
  }
}


#' @title Aggregated Connectedness Measures
#' @description This function aggregates the connectedness measures
#' @param dca Dynamic connectedness object
#' @param groups List of at least two group vectors
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
ConnectednessAggregate = function(dca, groups=list(c(1), c(2:k)), standardize=TRUE) {
  if (dca$approach=="Frequency" | dca$approach=="Joint") {
    stop(paste("Aggregated connectedness measures are not implemented for",dca$approach, "connectedness"))
  } else {
    ct = dca$CT
    date = dimnames(ct)[[3]]
    k = dim(ct)[1]
    m = length(groups)
    t = dim(ct)[3]
    
    NAMES_group = names(groups)
    if (is.null(NAMES_group)) {
      NAMES_group = paste0("GROUP", 1:m)
    }
    
    CT_group = array(0, c(m, m, t), dimnames=list(NAMES_group, NAMES_group, date))
    for (i in 1:m) {
      for (j in 1:m) {
        group_1 = groups[[i]]
        group_2 = groups[[j]]
        CT_group[i,j,] = apply(ct[group_1,group_2,,drop=FALSE],3,sum)/m
        CT_group[j,i,] = apply(ct[group_2,group_1,,drop=FALSE],3,sum)/m
      }
    }
    for (i in 1:dim(CT_group)[3]) {
      CT_group[,,i] = CT_group[,,i]/rowSums(CT_group[,,i])
    }
    
    TCI_group = array(NA, c(t,1,2), dimnames=list(as.character(date), c("TCI"), c("cTCI","TCI")))
    NPDC_group = NET_group = FROM_group = TO_group = array(NA, c(t, m), dimnames=list(date, NAMES_group))
    PCI_group = NPSO_group = INFLUENCE_group = array(NA, c(m, m, t), dimnames=list(NAMES_group, NAMES_group, date))
    for (i in 1:t) {
      dca = ConnectednessTable(CT_group[,,i])
      TCI_group[i,1,] = c(dca$cTCI, dca$TCI)
      TO_group[i,] = dca$TO
      FROM_group[i,] = dca$FROM
      NET_group[i,] = dca$NET
      NPDC_group[i,] = dca$NPDC
      PCI_group[,,i] = dca$PCI
      NPSO_group[,,i] = dca$NPSO
      INFLUENCE_group[,,i] = dca$INFLUENCE
    }
    TABLE = ConnectednessTable(CT_group)$TABLE
    
    return = list(TABLE=TABLE, CT=CT_group, TCI=TCI_group, NET=NET_group, TO=TO_group, FROM=FROM_group,
                  NPDC=NPDC_group, NPSO=NPSO_group, PCI=PCI_group, INFLUENCE=INFLUENCE_group,
                  approach="Aggregate")
  }
}


#' @title ConditionalConnectedness
#' @description This function computes the conditional connectedness table
#' @param dca Dynamic connectedness object
#' @param group Group vector
#' @return Get connectedness measures
#' @examples
#' #Replication of Chatzianzoniou, Gabauer and Stenfors (2022)
#' #data(cgs2022)
#' #cgs2022 = cgs2022[1:300,]
#' #dca = ConnectednessApproach(cgs2022, model="TVP-VAR",
#' #                              connectedness="Time",
#' #                             nlag=1, nfore=10, window.size=250,
#' #                             VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99,
#' #                              prior="BayesPrior")))
#' #cc = ConditionalConnectedness(dca, group=c(1,4,7,10,13,16))
#' #cc$TABLE
#' @references Chatziantoniou, I., Gabauer, D., & Stenfors, A. (2021). Independent Policy, Dependent Outcomes: A Game of Cross-Country Dominoes across European Yield Curves (No. 2021-06). University of Portsmouth, Portsmouth Business School, Economics and Finance Subject Group.
#' @author David Gabauer
#' @export
ConditionalConnectedness = function(dca, group=c(1,2,3)) {
  ct = dca$FEVD[group,group,,drop=FALSE]
  k = length(group)
  NAMES = dimnames(ct)[[1]]
  date = dimnames(ct)[[3]]
  t = length(date)

  TOTAL = cTOTAL = array(NA, c(t,1), dimnames=list(date,"TCI"))
  TO = FROM = NET = array(NA, c(t,k), dimnames=list(date,NAMES))
  FEVD = NPSO = array(NA, c(k,k,t), dimnames=list(NAMES,NAMES,date))
  for (i in 1:t) {
    cc = ConnectednessTable(ct[,,i]/rowSums(ct[,,i]))
    FEVD[,,i] = cc$FEVD
    TOTAL[i,] = cc$TOTAL
    cTOTAL[i,] = cc$cTOTAL
    TO[i,] = cc$TO
    FROM[i,] = cc$FROM
    NET[i,] = cc$NET
    NPSO[,,i] = cc$NPSO
  }
  TABLE = ConnectednessTable(FEVD/100)$TABLE
  return = list(TABLE=TABLE, FEVD=FEVD, TOTAL=TOTAL, cTOTAL=cTOTAL, NET=NET, TO=TO, FROM=FROM, NPSO=NPSO)
}

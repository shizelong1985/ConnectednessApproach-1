
#' @title Baruník and Křehlík (2018) frequency connectedness approach
#' @description This function calculates the Baruník and Křehlík (2018) frequency connectedness measures.
#' @param Phi VAR coefficient matrix
#' @param Sigma Residual variance-covariance matrix
#' @param nfore H-step ahead forecast horizon
#' @param partition Frequency spectrum
#' @param generalized Orthorgonalized/generalized FEVD
#' @param orth Orthorgonalized shocks
#' @param no.corr Uncorrelated shocks
#' @return Get connectedness measures
#' @examples
#' # Replication of frequencyConnectedness (https://github.com/tomaskrehlik/frequencyConnectedness)
#' #data("exampleSim")
#' #x = zoo(exampleSim[1:600,], order.by=1:600)
#' #partition = c(pi+0.00001, pi/4, 0)
#' #dca = ConnectednessApproach(x, model="VAR",
#' #                            connectedness="Frequency",
#' #                            nlag=2, nfore=100,
#' #                            Connectedness_config=list(
#' #                            FrequencyConnectedness=list(partition=partition, generalized=TRUE)))
#' #dca$TABLE
#' @import frequencyConnectedness
#' @references
#' Baruník, J., & Křehlík, T. (2018). Measuring the frequency dynamics of financial connectedness and systemic risk. Journal of Financial Econometrics, 16(2), 271-296.
#' @author David Gabauer
#' @export
FrequencyConnectedness = function(Phi, Sigma, nfore, partition, generalized=TRUE, orth=FALSE, no.corr=FALSE) {
  NAMES = colnames(Sigma)
  if (length(dim(Phi))==2) {
    Phi = array(Phi, c(nrow(Phi),ncol(Phi),1))
  }
  if (length(dim(Sigma))==2) {
    Sigma = array(Sigma, c(nrow(Sigma),ncol(Sigma),1))
  }

  k = dim(Sigma)[1]
  t = dim(Sigma)[3]

  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  periods = pi/partition
  period_names = NULL
  for (i in 1:(length(periods)-1)) {
    period_names = c(period_names, paste0(periods[i], "-", periods[i+1]))
  }

  scenarios = c("ABS","WTH")
  date = as.character(dimnames(Sigma)[[3]])
  interval = length(period_names)
  new_p = getPartition(partition, nfore)
  range = sort(unique(do.call(c, new_p)))

  TOTAL = cTOTAL = array(NA, c(t,interval,2), dimnames=list(as.character(date), period_names, scenarios))
  NPDC = array(NA, c(t, k, interval), dimnames=list(date, NAMES, period_names))
  NET = FROM = TO = array(NA, c(t, k, interval, 2), dimnames=list(date, NAMES, period_names, scenarios))
  FEVD = NPSO = array(NA, c(k, k, t, interval, 2), dimnames=list(NAMES, NAMES, date, period_names, scenarios))
  PCI = INFLUENCE = array(NA, c(k, k, t, interval), dimnames=list(NAMES, NAMES, date, period_names))
  pb = progress_bar$new(total=t)
  for (i in 1:t) {
    decomp = FEVD(Phi=Phi[,,i], Sigma=Sigma[,,i], nfore=nfore, generalized=generalized, type="frequency", range=range)$FEVD
    for (ij in 1:length(decomp)) {
      rownames(decomp[[ij]]) = colnames(decomp[[ij]]) = 1:ncol(Sigma)
    }
    tables = lapply(new_p, function(j) Reduce('+', decomp[j]))
    #print(tables)
    for (j in 1:interval) {
      dca = ConnectednessTable(tables[[j]])
      FEVD[,,i,j,1] = dca$FEVD
      TO[i,,j,1] = dca$TO
      FROM[i,,j,1] = dca$FROM
      NET[i,,j,1] = dca$NET
      TOTAL[i,j,1] = dca$TOTAL
      cTOTAL[i,j,1] = dca$cTOTAL
      NPSO[,,i,j,1] = dca$NPSO

      dca = ConnectednessTable(tables[[j]]/sum(sum(tables[[j]]))*k)
      FEVD[,,i,j,2] = dca$FEVD
      TO[i,,j,2] = dca$TO
      FROM[i,,j,2] = dca$FROM
      NET[i,,j,2] = dca$NET
      TOTAL[i,j,2] = dca$TOTAL
      cTOTAL[i,j,2] = dca$cTOTAL
      NPSO[,,i,j,2] = dca$NPSO

      PCI[,,i,j] = dca$PCI
      INFLUENCE[,,i,j] = dca$INFLUENCE
      NPDC[i,,j] = dca$NPDC
    }
    pb$tick()
  }

  TABLE = array(NA,c(k+4,k+1,interval,2), dimnames=list(c(NAMES, "TO", "Inc.Own", "Net", "NPDC"), c(NAMES, "FROM"), period_names, c("ABS","WTH")))
  for (i in 1:interval) {
    TABLE[,,i,1] = ConnectednessTable(apply(FEVD[,,,i,1], c(1,2), mean)/100)$TABLE
    TABLE[,,i,2] = ConnectednessTable(apply(FEVD[,,,i,2], c(1,2), mean)/100)$TABLE
  }
  return = list(TABLE=TABLE, FEVD=FEVD, TOTAL=TOTAL, cTOTAL=cTOTAL, TO=TO, FROM=FROM,
                NET=NET, NPDC=NPDC, NPSO=NPSO, PCI=PCI, INFLUENCE=INFLUENCE)
}

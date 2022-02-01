
#' @title Baruník and Křehlík (2018) frequency connectedness approach
#' @description This function calculates the Baruník and Křehlík (2018) frequency connectedness measures.
#' @param Phi VAR coefficient matrix
#' @param Sigma Residual variance-covariance matrix
#' @param nfore H-step ahead forecast horizon
#' @param partition Frequency spectrum
#' @param generalized Orthorgonalized/generalized FEVD
#' @param scenario ABS or WTH
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
FrequencyConnectedness = function(Phi, Sigma, nfore, partition=c(pi,pi/2,0), generalized=TRUE, orth=FALSE, no.corr=FALSE, scenario="ABS") {
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
  periods = round(pi/partition)
  period_names = NULL
  for (i in 1:(length(periods)-1)) {
    period_names = c(period_names, paste0(periods[i], "-", periods[i+1]))
  }

  date = as.character(dimnames(Sigma)[[3]])
  interval = length(period_names)
  new_p = frequencyConnectedness::getPartition(partition, nfore)
  range = sort(unique(do.call(c, new_p)))

  TCI = array(NA, c(t,interval, 2), dimnames=list(as.character(date), period_names, c("cTCI","TCI")))
  NPDC = array(NA, c(t, k, interval), dimnames=list(date, NAMES, period_names))
  NET = FROM = TO = array(NA, c(t, k, interval), dimnames=list(date, NAMES, period_names))
  CT = NPSO = array(NA, c(k, k, t, interval), dimnames=list(NAMES, NAMES, date, period_names))
  PCI = INFLUENCE = array(NA, c(k, k, t, interval), dimnames=list(NAMES, NAMES, date, period_names))
  pb = progress_bar$new(total=t)
  for (i in 1:t) {
    decomp = FEVD(Phi=Phi[,,i], Sigma=Sigma[,,i], nfore=nfore, generalized=generalized, type="frequency", range=range)$FEVD
    for (ij in 1:length(decomp)) {
      rownames(decomp[[ij]]) = colnames(decomp[[ij]]) = 1:ncol(Sigma)
    }
    tables = lapply(new_p, function(j) Reduce('+', decomp[j]))
    for (j in 1:interval) {
      if (scenario=="ABS") {
        dca = ConnectednessTable(tables[[j]])
        CT[,,i,j] = dca$FEVD
        TO[i,,j] = dca$TO
        FROM[i,,j] = dca$FROM
        NET[i,,j] = dca$NET
        TCI[i,j,] = c(dca$cTCI, dca$TCI)
        NPSO[,,i,j] = dca$NPSO
        PCI[,,i,j] = dca$PCI
        INFLUENCE[,,i,j] = dca$INFLUENCE
        NPDC[i,,j] = dca$NPDC
      } else if (scenario=="WTH") {
        dca = ConnectednessTable(tables[[j]]/sum(sum(tables[[j]]))*k)
        CT[,,i,j] = dca$FEVD
        TO[i,,j] = dca$TO
        FROM[i,,j] = dca$FROM
        NET[i,,j] = dca$NET
        TCI[i,j,] = c(dca$cTCI, dca$TCI)
        NPSO[,,i,j] = dca$NPSO
        PCI[,,i,j] = dca$PCI
        INFLUENCE[,,i,j] = dca$INFLUENCE
        NPDC[i,,j] = dca$NPDC
      }
    }
    pb$tick()
  }

  TABLE = array(NA,c(k+4,k+1,interval), dimnames=list(c(NAMES, "TO", "Inc.Own", "Net", "NPDC"), c(NAMES, "FROM"), period_names))
  for (i in 1:interval) {
    TABLE[,,i] = ConnectednessTable(CT[,,,i]/100)$TABLE
  }
  # still TCI and cTCI
  return = list(TABLE=TABLE, CT=CT/100, TCI=TCI, TO=TO, FROM=FROM,
                NET=NET, NPDC=NPDC, NPSO=NPSO, PCI=PCI, INFLUENCE=INFLUENCE, approach="Frequency")
}

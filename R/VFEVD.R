
#' @title Generalized volatility impulse response functions and forecast error variance decomposition
#' @description This function provides the volatility impulse responses and the forecast error variance decomposition of DCC-GARCH models.
#' @param fit Fitted DCC-GARCH model
#' @param nfore H-step ahead forecast horizon
#' @param standardize Boolean. Standardizing GIRF
#' @return Get volatility impulse response functions and forecast error variance decomposition
#' @examples
#' #data(g2020)
#' #ugarch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
#' #                         variance.model=list(garchOrder=c(1,1), model="sGARCH"),
#' #                         distribution.model="norm")
#' #mgarch.spec = dccspec(uspec=multispec(replicate(ncol(g2020), ugarch.spec)),
#' #                      dccOrder=c(1,1), distribution="mvnorm")
#' #fit = dccfit(mgarch.spec, data=g2020)
#' #VFEVD(fit, nfore=100, standardize=FALSE)$TABLE
#' @import rmgarch
#' @references
#' Engle, R. (2002). Dynamic conditional correlation: A simple class of multivariate generalized autoregressive conditional heteroskedasticity models. Journal of Business & Economic Statistics, 20(3), 339-350.\\
#' Gabauer, D. (2020). Volatility impulse response analysis for DCC‐GARCH models: The role of volatility transmission mechanisms. Journal of Forecasting, 39(5), 788-796.
#' @author David Gabauer
#' @export
VFEVD = function(fit, nfore=100, standardize=FALSE) {
  NAMES = fit@model$modeldata$asset.names
  H = rcov(fit)
  R = rcor(fit)
  R.bar = apply(R,1:2,mean)
  Q.bar = fit@mfit$Qbar
  t = dim(H)[3]
  k = dim(H)[1]
  alpha = array(0,c(k,k,nfore))
  alpha[,,1] = diag(fit@mfit$matcoef[c(seq(3,(4*k),4)),1])
  beta = diag(fit@mfit$matcoef[c(seq(4,(4*k),4)),1])
  ALPHA = fit@mfit$matcoef[(4*k+1),1]
  BETA = fit@mfit$matcoef[(4*k+2),1]

  H.hat = array(0,c(k,k,nfore+1))
  VIRF = H.hat.shock = H.hat.no_shock = array(0,c(k,k,t,nfore+1))
  e = diag(k)
  for (i in 1:t) {
    H.hat[,,1] = H[,,i]
    Q.hat = H.hat
    Q.hat[,,1] = fit@mfit$Q[[i]]
    for (j in 1:nfore) {
      H.hat[,,j+1] = (alpha[,,j])%*%e^2 + beta%*%H.hat[,,j]
      D = diag(diag(H.hat[,,j+1])^0.5)
      u = D%*%e
      if (j==1) {
        Q.hat[,,2] = (1-ALPHA-BETA)*Q.bar + ALPHA*crossprod(u) + BETA*H.hat[,,1]
      } else {
        Q.hat[,,j+1] = (1-ALPHA-BETA)*Q.bar + (ALPHA+BETA)*Q.hat[,,j]
      }
      R.hat = diag(1/(diag(Q.hat[,,j+1])^0.5))%*%Q.hat[,,j+1]%*%(diag(1/diag(Q.hat[,,j+1])^0.5))
      H.hat[,,j+1] = D%*%R.hat%*%D
    }
    H.hat.shock[,,i,] = H.hat
  }
  if (standardize) {
    e = 0*diag(k)
    for (i in 1:t) {
      H.hat[,,1] = H[,,i]
      Q.hat = H.hat
      Q.hat[,,1] = fit@mfit$Q[[i]]
      for (j in 1:nfore) {
        H.hat[,,j+1] = beta%*%H.hat[,,j]
        D = diag(diag(H.hat[,,j+1])^0.5)
        if (j==1) {
          Q.hat[,,2] = (1-ALPHA-BETA)*Q.bar + BETA*H.hat[,,1]
        } else {
          Q.hat[,,j+1] = (1-ALPHA-BETA)*Q.bar+(ALPHA+BETA)*Q.hat[,,j]
        }
        R.hat = diag(1/(diag(Q.hat[,,j+1])^0.5))%*%Q.hat[,,j+1]%*%(diag(1/diag(Q.hat[,,j+1])^0.5))
        H.hat[,,j+1] = D%*%R.hat%*%D
      }
      H.hat.no_shock[,,i,] = H.hat
    }
  }
  for (i in 1:t) {
    VIRF[,,i,] = H.hat.shock[,,i,] - H.hat.no_shock[,,i,]
  }
  date = dimnames(H)[[3]]
  VFEVD = array(NA, c(k,k,t), dimnames=list(NAMES,NAMES,date))
  for (i in 1:t) {
    num = apply(VIRF[,,i,]^2,1:2,sum)
    den = c(apply(num,1,sum))
    fevd = t(num)/den
    VFEVD[,,i] = (fevd/apply(fevd, 1, sum))
  }
  TCI = array(NA, c(t,2),dimnames=list(date,c("cTCI","TCI")))
  PCI = INFLUENCE = NPSO = array(NA, c(k,k,t),dimnames=list(NAMES,NAMES,date))
  TO = FROM = NET = array(NA, c(t,k),dimnames=list(date,NAMES))
  for (i in 1:t) {
    dca = ConnectednessTable(VFEVD[,,i])
    NET[i,] = dca$NET
    TO[i,] = dca$TO
    FROM[i,] = dca$FROM
    NPSO[,,i] = dca$NPSO
    PCI[,,i] = dca$PCI
    INFLUENCE[,,i] = dca$INFLUENCE
    TCI[i,] = c(dca$cTCI, dca$TCI)
  }
  TABLE = ConnectednessTable(VFEVD)$TABLE
  return = list(TABLE=TABLE, FEVD=VFEVD, IRF=VIRF, TCI=TCI, NET=NET, TO=TO, FROM=FROM, NPSO=NPSO, PCI=PCI, INFLUENCE=INFLUENCE, approach="volatility")
}

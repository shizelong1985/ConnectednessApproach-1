#' @title Summary Statistics
#' @description Get summary statistics
#' @param x zoo data matrix
#' @param correlation correlation coefficient: "pearson", "kendall", "spearman".
#' @param portmanteau portmanteau statistics: "Box-Pierce", "Ljung-Box", "Monti"
#' @param nlag number of lags for Weighted Portmanteau statistics
#' @param digit digit Number of decimal places
#' @return Get summary statistics
#' @examples
#' #data(dy2012)
#' #SummaryStatistics(dy2012)
#' @author David Gabauer
#' @importFrom stats var
#' @importFrom stats t.test
#' @importFrom stats cor.test
#' @export
SummaryStatistics = function(x, portmanteau="Ljung-Box", correlation="kendall", nlag=20, digit=3) {
  x = as.matrix(x)
  k = ncol(x)
  NAMES = colnames(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  moments = matrix(NA, ncol=k, nrow=16)
  colnames(moments) = NAMES
  rownames(moments) = c("Mean", "", "Variance", "", "Skewness", "", "Ex.Kurtosis", "",
                        "JB","","ERS","",paste0("Q(",nlag,")"), "", paste0("Q2(",nlag,")"), "")
  for (i in 1:k) {
    ttest = t.test(x[,i])
    moments[1,i] = ttest$estimate
    moments[2,i] = ttest$p.value
    moments[3,i] = var(x[,i])
    moments[4,i] = 0
    skew = moments::agostino.test(x[,i])
    moments[5,i] = skew$statistic[1]
    moments[6,i] = skew$p.value
    kurt = moments::anscombe.test(x[,i])
    moments[7,i] = kurt$statistic[1]-3
    moments[8,i] = kurt$p.value
    jb = moments::jarque.test(x[,i])
    moments[9,i] = jb$statistic
    moments[10,i] = jb$p.value
    ers = urca::ur.ers(x[,i],type="DF-GLS",model="constant")
    moments[11,i] = ers@teststat
    moments[12,i]= ers@testreg$coefficients[1,4]
    bt = WeightedPortTest::Weighted.Box.test(x[,i], type=portmanteau, lag=nlag)
    moments[13,i] = bt$statistic
    moments[14,i] = bt$p.value
    bt2 = WeightedPortTest::Weighted.Box.test(x[,i], type=portmanteau, lag=nlag, sqrd.res=TRUE)
    moments[15,i] = bt2$statistic
    moments[16,i] = bt2$p.value
  }

  cc = seq(2,nrow(moments),2)
  moments = round(moments, digit)
  SumStat = moments
  for (j in 1:k) {
    for (i in 1:length(cc)) {
      i = cc[i]
      if (moments[i,j]<=0.01) {
        SumStat[(i-1),j] = paste(format(round(moments[(i-1),j],digit),nsmall=digit),"***",sep="")
        SumStat[i,j] = paste("(",format(round(moments[i,j],digit),nsmall=digit),")",sep="")
      } else if (moments[i,j]<=0.05) {
        SumStat[(i-1),j] = paste(format(round(moments[(i-1),j],digit),nsmall=digit),"**",sep="")
        SumStat[i,j] = paste("(",format(round(moments[i,j],digit),nsmall=digit),")",sep="")
      } else if (moments[i,j]<=0.10) {
        SumStat[(i-1),j] = paste(format(round(moments[(i-1),j],digit),nsmall=digit),"*",sep="")
        SumStat[i,j] = paste("(",format(round(moments[i,j],digit),nsmall=digit),")",sep="")
      } else {
        SumStat[(i-1),j] = format(round(moments[(i-1),j],digit),nsmall=digit)
        SumStat[i,j] = paste("(",format(round(moments[i,j],digit),nsmall=digit),")",sep="")
      }
    }
  }

  for (j in 1:k) {
    i = 11
    if (moments[i,j]<=-2.57) {
      SumStat[i,j] = paste(format(round(moments[i,j],digit), nsmall=digit),"***",sep="")
    } else if (moments[i,j]<=-1.96) {
      SumStat[i,j] = paste(format(round(moments[i,j],digit), nsmall=digit),"**",sep="")
    } else if (moments[i,j]<=-1.62) {
      SumStat[i,j] = paste(format(round(moments[i,j],digit), nsmall=digit),"*",sep="")
    } else {
      SumStat[i,j] = format(round(moments[i,j],digit), nsmall=digit)
    }
  }

  Cor = array(NA, c(k, k), dimnames=list(NAMES, NAMES))
  for (i in 1:k) {
    for (j in 1:k) {
      res = cor.test(x[,i], x[,j], method=correlation)
      est = res$estimate
      p.val = res$p.value
      if (p.val<=0.01) {
        Cor[i,j] = paste(format(round(est,digit), nsmall=digit),"***",sep="")
      } else if (p.val<=0.05) {
        Cor[i,j] = paste(format(round(est,digit), nsmall=digit),"**",sep="")
      } else if (p.val<=-0.10) {
        Cor[i,j] = paste(format(round(est,digit), nsmall=digit),"*",sep="")
      } else {
        Cor[i,j] = format(round(est,digit), nsmall=digit)
      }

    }
  }
  SumStat = rbind(SumStat, NAMES, Cor)
  rownames(SumStat)[nrow(moments)+1] = paste(correlation)
  SumStat[-4,]
}

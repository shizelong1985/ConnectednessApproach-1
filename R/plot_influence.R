#' @title Dynamic influence connectedness plot
#' @description Visualize dynamic influence connectedness
#' @param ca Connectedness object
#' @param col Color
#' @param save Save plot as pdf
#' @param ylim A vector including the lower and upper limit of the y-axis
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#' @return Return connectedness plot
#' @export
plot_influence = function(ca, col="steelblue4", save=FALSE, ylim=c(NULL, NULL), ...) {
  if (!dir.exists('./Results')){
    dir.create('./Results')
  }
  x = ca$INFLUENCE
  date = as.Date(dimnames(x)[[3]])
  t = length(date)
  k = ncol(x)
  NAMES = colnames(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  if (is.null(ylim[0])) {
    lower = min(x)
  }
  if (is.null(ylim[1])) {
    upper = max(x)
  }
  
  kk = k*(k-1)/2
  k_row = ceiling(sqrt(kk))
  k_col = ceiling(kk/k_row)
  if (save) pdf(file=paste0("./Results/INFLUENCE.pdf"), width=10, height=7)
  par(mfcol=c(k_row, k_col), oma=c(0,0,0,0) + 0.5, mar = c(1,1,1,1) + .5, mgp=c(1, 0.4, 0))
  for (j in 1:k) {
    for (i in 1:k) {
      if (i>j) {
        plot(date, x[i,j,], type="l", main=paste(NAMES[j],"-",NAMES[i]), las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper), ...)
        grid(NA, NULL, lty=2)
        polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x[i,j,])),col=col, border=col)
        abline(h=0, lty=3)
        box()
      }
    }
  }
  if (save) dev.off()
}

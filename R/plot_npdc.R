#' @title Dynamic net pairwise transmission plot
#' @description Visualize dynamic net total directional connectedness
#' @param ca Connectedness object
#' @param col Color
#' @param save Save plot as pdf
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#' @return Return connectedness plot
#' @export
plot_npdc = function(ca, col="steelblue4", save=FALSE, ...) {
  if (!dir.exists('./Results')){
    dir.create('./Results')
  }
  x = ca$NPDC
  date = as.Date(rownames(x))
  t = length(date)
  k = ncol(x)
  NAMES = colnames(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  k_row = ceiling(sqrt(k))
  k_col = ceiling(k/k_row)

  if (save) pdf(file=paste0("./Results/NPDC.pdf"), width=10, height=7)
  par(mfcol=c(k_row,k_col), oma=c(0,0,0,0) + 0.5, mar = c(1,1,1,1) + .5, mgp=c(1, 0.4, 0))
  for (i in 1:k) {
    plot(date, x[,i], type="l", main=NAMES[i], las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(0,k-1), ...)
    grid(NA, NULL, lty=2)
    polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x[,i])),col=col, border=col)
    abline(h=0, lty=3)
    box()
  }
  if (save) dev.off()
}


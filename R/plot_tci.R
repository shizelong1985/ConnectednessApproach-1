
#' @title Dynamic total connectedness plot
#' @description Visualize dynamic total connectedness
#' @param ca Connectedness object
#' @param col Color
#' @param save Save plot as pdf
#' @param ylim A vector including the lower and upper limit of the y-axis
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#' @return Return connectedness plot
#' @import graphics
#' @import grDevices
#' @export
plot_tci = function(ca, col="steelblue4", save=FALSE, ylim=c(NULL, NULL), ...) {
  if (!dir.exists('./Results')){
    dir.create('./Results')
  }
  x = ca$TCI
  date = as.Date(rownames(x))
  t = length(date)
  if (!is.null(ca$cTCI)) {
    x = cbind(ca$cTCI, x)
  } else {
    x = as.matrix(ca$TCI)
  }
  k = ncol(x)
  if (is.null(ylim[0])) {
    lower = min(x)
  }
  if (is.null(ylim[1])) {
    upper = max(x)
  }

  if (save) pdf(file=paste0("./Results/TCI.pdf"), width=10, height=5)
  par(mfrow=c(1,1), oma=c(0,0,0,0) + 0.5, mar = c(1,1,1,1) + .5, mgp=c(1, 0.4, 0))
  plot(date, x[,1], type="l", main="", las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper), ...)
  grid(NA, NULL, lty=2)
  polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x[,1])),col=col, border=col)
  box()
  if (!is.null(ca$cTCI)) {
    lines(date, x[,2], col="black", lty=2)
    legend("topleft", c("TCI corrected", "TCI"), fill=c(col, "black"))
  }
  if (save) dev.off()
}

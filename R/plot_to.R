#' @title Dynamic to total directional connectedness plot
#' @description Visualize dynamic to total directional connectedness
#' @param ca Connectedness object
#' @param save Save plot as pdf
#' @param path Path where plots should be saved
#' @param ylim A vector including the lower and upper limit of the y-axis
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#' @return Return connectedness plot
#' @export
plot_to = function(ca, save=FALSE, path='./Results', ylim=c(NULL, NULL), ...) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
  x = ca$TO
  date = as.Date(rownames(x))
  t = length(date)
  k = ncol(x)
  NAMES = colnames(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  k_row = ceiling(sqrt(k))
  k_col = ceiling(k/k_row)
  lower = ylim[1]
  upper = ylim[2]
  
  if (save) pdf(file=paste0(path, "/TO.pdf"), width=10, height=7)
  par(mfcol=c(k_row,k_col), oma=c(0,0,0,0) + 0.5, mar = c(1,1,1,1) + .5, mgp=c(1, 0.4, 0))
  if (ca$approach!="Frequency") {
    if (is.null(lower)) {
      lower = min(x)
    }
    if (is.null(upper)) {
      upper = max(x)
    }
    for (i in 1:k) {
      if (length(dim(x))==2) {
        plot(date, x[,i], type="l", main=NAMES[i], las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper), ...)
        grid(NA, NULL, lty=2)
        polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x[,i])),col=1, border=1)
        abline(h=0, lty=3)
        box()
      } else {
        plot(date, x[,i,1], type="l", main=NAMES[i], las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper), ...)
        grid(NA, NULL, lty=2)
        polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x[,i,1])),col=1, border=1)
        for (j in 1:dim(x)[3]) {
          lines(date, x[,i,j], col=j, lty=2)
        }
        legend("topleft", colnames(x[,1,]), fill=1:dim(x)[3], bty="n")
        abline(h=0, lty=3)
        box()
      }
    }
  } else {
    for (i in 1:k) {
      x_ = x[,i,]
      if (is.null(lower)) {
        lower = min(x)
      }
      if (is.null(upper)) {
        upper = max(apply(x,1:2,sum))
      }
      plot(date, apply(x_,1,sum), type="l", main=NAMES[i], las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper))#, ...)
      grid(NA, NULL, lty=2)
      polygon(c(date,rev(date)),c(c(rep(0,t)),rev(apply(x_,1,sum))),col=1, border=1)
      for (j in ncol(x_):1) {
        polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x_[,j])),col=j+1, border=j+1)
      }
      for (j in 1:ncol(x_)) {
        lines(date, x_[,j],col=j+1)
      }
      abline(h=0, lty=3)
      legend("topleft", c("total", colnames(x_)), fill=c(1:(ncol(x_)+1)), bty="n")
      box()
    }
  }
  if (save) dev.off()
}

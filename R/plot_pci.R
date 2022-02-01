#' @title Dynamic pairwise connectedness plot
#' @description Visualize dynamic pairwise connectedness
#' @param ca Connectedness object
#' @param save Save plot as pdf
#' @param path Path where plots should be saved
#' @param ylim A vector including the lower and upper limit of the y-axis
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#' @return Return connectedness plot
#' @export
plot_pci = function(ca, save=FALSE, path='./Results', ylim=c(NULL, NULL), ...) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
  x = ca$PCI
  if (is.null(x)) {
    stop(paste(ca$approach, "has no PCI."))
  }
  date = as.Date(dimnames(x)[[3]])
  t = length(date)
  k = ncol(x)
  NAMES = colnames(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  lower = ylim[1]
  upper = ylim[2]
  
  kk = k*(k-1)/2
  k_row = ceiling(sqrt(kk))
  k_col = ceiling(kk/k_row)
  if (save) pdf(file=paste0(path, "/PCI.pdf"), width=10, height=7)
  par(mfcol=c(k_row, k_col), oma=c(0,0,0,0) + 0.5, mar = c(1,1,1,1) + .5, mgp=c(1, 0.4, 0))
  if (ca$approach!="Frequency") {
    if (is.null(lower)) {
      lower = min(x)
    }
    if (is.null(upper)) {
      upper = max(x)
    }
    
    for (j in 1:k) {
      for (i in 1:k) {
        if (i>j) {
          plot(date, x[i,j,], type="l", main=paste(NAMES[j],"-",NAMES[i]), las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper), ...)
          grid(NA, NULL, lty=2)
          polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x[i,j,])),col=1, border=1)
          abline(h=0, lty=3)
          box()
        }
      }
    }
  } else {
    for (j in 1:k) {
      for (i in 1:k) {
        if (i>j) {
          x_ = x[i,j,,]
          x[which(x>=99.99999,arr.ind=T)] = 0
          if (is.null(lower)) {
            lower = min(x)
          }
          if (is.null(upper)) {
            upper = max(x)
          }
          plot(date, x_[,1], type="l", main=paste(NAMES[j],"-",NAMES[i]), las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper), ...)
          grid(NA, NULL, lty=2)
          polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x_[,1])),col=1, border=1)
          for (l in ncol(x_):1) {
            polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x_[,l])),col=l, border=l)
          }
          for (l in 1:ncol(x_)) {
            lines(date, x_[,l],col=l)
          }
          abline(h=0, lty=3)
          legend("topleft", colnames(x_), fill=1:ncol(x_), bty="n")
          box()
        }
      }
    }
  }
  if (save) dev.off()
}

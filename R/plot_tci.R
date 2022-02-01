
#' @title Dynamic total connectedness plot
#' @description Visualize dynamic total connectedness
#' @param ca Connectedness object
#' @param save Save plot as pdf
#' @param path Path where plots should be saved
#' @param ylim A vector including the lower and upper limit of the y-axis
#' @param corrected Visualizing standard or corrected TCI values
#' @param ... Arguments to be passed to methods, such as graphical parameters (see par).
#' @return Return connectedness plot
#' @import graphics
#' @import grDevices
#' @export
plot_tci = function(ca, save=FALSE, path='./Results', ylim=c(NULL, NULL), corrected=FALSE, ...) {
  if (save) {
    if (!dir.exists(path)) {
      dir.create(path)
    }
  }
  x = ca$TCI
  date = as.Date(rownames(x))
  t = length(date)
  if (length(dim(x))==2) {
    x = array(x, c(nrow(x),1,ncol(x)))
  }
  ind = 1
  if (!corrected) {
    ind = 2
  }
  k = ncol(x)
  lower = ylim[1]
  upper = ylim[2]
  
  if (save) pdf(file=paste0(path, "/TCI.pdf"), width=10, height=5)
  par(mfrow=c(1,1), oma=c(0,0,0,0) + 0.5, mar = c(1,1,1,1) + .5, mgp=c(1, 0.4, 0))
  if (ca$approach!="Frequency") {
    iter = dim(x)[2]
    if (iter==1) {
      if (is.null(lower)) {
        lower = min(x[,1,ind])
      }
      if (is.null(upper)) {
        upper = max(x[,1,ind])
      }
      plot(date, as.numeric(x[,1,ind]), type="l", main="", las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper))#, ...)
      grid(NA, NULL, lty=2)
      polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x[,1,ind])),col=1, border=1)
    } else {
      if (is.null(lower)) {
        lower = min(x[,,ind])
      }
      if (is.null(upper)) {
        upper = max(apply(x[,,ind],1,sum))
      }
      plot(date, apply(x[,,ind,drop=FALSE],1,sum), type="l", main="", las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper))#, ...)
      grid(NA, NULL, lty=2)
      polygon(c(date,rev(date)),c(c(rep(0,t)),rev(apply(x[,,ind,drop=FALSE],1,sum))),col=1, border=1)
      for (j in iter:1) {
        lines(date, x[,j,ind], col=j+1, lty=2)
      }
      legend_names = c("total",colnames(x[,,ind,drop=FALSE]))
      fill = c(1:(iter+1))
      if (ca$approach=="Decompose") {
        gTCI = ca$gTCI[,,ind]
        legend_names = c(legend_names, colnames(gTCI))
        for (j in 1:ncol(gTCI)) {
          lines(date, gTCI[,j], col=iter+1+j, lty=2)
        }
        fill = 1:(iter+ncol(gTCI)+1)
      }
      legend("topleft", legend_names, fill=fill, bty="n")
    }
    box()
  } else {
    x_ = x
    if (is.null(lower)) {
      lower = min(x_)
    }
    if (is.null(upper)) {
      upper = max(apply(x_[,,ind],1,sum))
    }
    
    plot(date, apply(x_[,,ind],1,sum), type="l", main="", las=1, xlab="", ylab="", xaxs="i", yaxs="i", tck=-0.02, ylim=c(lower,upper), ...)
    grid(NA, NULL, lty=2)
    polygon(c(date,rev(date)),c(c(rep(0,t)),rev(apply(x_[,,ind],1,sum))),col=1, border=1)
    for (j in dim(x_)[2]:1) {
      polygon(c(date,rev(date)),c(c(rep(0,t)),rev(x_[,j,ind])),col=j+1, border=j+1)
    }
    legend("topleft", c("total", colnames(x_[,,ind])), fill=c(1:(dim(x_)[2]+1)), bty="n")
    for (j in 1:ncol(x_)) {
      lines(date, x_[,j,ind],col=j+1)
    }
    abline(h=0, lty=3)
    box()
  }
  if (save) dev.off()
}

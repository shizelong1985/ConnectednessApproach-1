
#' @title Bayes Prior
#' @description Get Bayes Prior
#' @param x zoo data matrix
#' @param nlag Lag length
#' @return Get VAR coefficients and variance-covariance matrix
#' @examples
#' #data(dy2012)
#' #prior = BayesPrior(dy2012, nlag=1)
#' @references Primiceri, G. E. (2005). Time varying structural vector autoregressions and monetary policy. The Review of Economic Studies, 72(3), 821-852.
#' @author David Gabauer
#' @export
BayesPrior = function(x, nlag){
  fit = VAR(x, configuration=list(nlag=nlag))
  bprior = c(fit$B)
  Vprior = diag(c(fit$se))^2
  return=list(bprior=bprior, Vprior=Vprior, Q=fit$Q[,,1])
}

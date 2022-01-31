#' @title Partial Contemporaneous Correlations
#'
#' @description Get partial contemporaneous correlations
#' @param fit ghjghj
#' @return Get partial contemporaneous correlations
#' @examples
#' #data(dy2012)
#' #fit = VAR(dy2012, configuration=list(nlag=1))
#' #pcc = PartialCorrelations(fit)
#' @references Dahlhaus, R., & Eichler, M. (2003). Causality and graphical models in time series analysis. Oxford Statistical Science Series, 115-137.
#' @author David Gabauer
#' @export
PartialCorrelations = function (fit) {
  pcc = sigma = fit$Q
  for (l in 1:dim(sigma)[3]) {
    precision = solve(sigma[,,l])
    theta = diag(1/sqrt(diag(precision)))
    pcc[,,l] = -theta %*% precision %*% theta
  }
  return(pcc)
}

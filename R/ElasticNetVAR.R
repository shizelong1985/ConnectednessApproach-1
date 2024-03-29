
#' @title Elastic Net Vector autoregression
#' @description Estimation of a VAR using equation-by-equation LASSO, Ridge or Elastic Net regressions.
#' @param x zoo data matrix
#' @param nlag Lag length
#' @param nfolds N-fold cross validation
#' @param loss Loss function
#' @param alpha LASSO is alpha equal 1 and Ridge if alpha equal 0
#' @param n_alpha Steps between 0 and 1. If alpha is NULL alpha is estimated based upon loss and nfolds
#' @param configuration Model configuration
#' @return Estimate VAR model
#' @examples
#' #data(dy2012)
#' #fit = ElasticNetVAR(dy2012, configuration=list(nlag=1, alpha=1, nfolds=10, loss="mae"))
#' @import glmnet
#' @importFrom stats predict
#' @references
#' Tibshirani, R., Bien, J., Friedman, J., Hastie, T., Simon, N., Taylor, J., & Tibshirani, R. J. (2012). Strong rules for discarding predictors in lasso‐type problems. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 74(2), 245-266.\\
#' Hoerl, A. E., & Kennard, R. W. (1970). Ridge regression: Biased estimation for nonorthogonal problems. Technometrics, 12(1), 55-67.\\
#' Zou, H., & Hastie, T. (2005). Regularization and variable selection via the elastic net. Journal of the royal statistical society: series B (statistical methodology), 67(2), 301-320.\\
#' Demirer, M., Diebold, F. X., Liu, L., & Yilmaz, K. (2018). Estimating global bank network connectedness. Journal of Applied Econometrics, 33(1), 1-15.\\
#' Gabauer, D., Gupta, R., Marfatia, H., & Miller, S. M. (2020). Estimating US Housing Price Network Connectedness: Evidence from Dynamic Elastic Net, Lasso, and Ridge Vector Autoregressive Models. Lasso, and Ridge Vector Autoregressive Models (July 26, 2020).
#' @author David Gabauer
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet predict.glmnet
#' @export
ElasticNetVAR = function(x, configuration=list(nlag=1, nfolds=10, loss="mae", alpha=NULL, n_alpha=10)) {
  nlag = configuration$nlag
  loss = configuration$loss
  alpha = configuration$alpha
  nfolds = configuration$nfolds
  n_alpha = configuration$n_alpha
  if (class(x)!="zoo") {
    print("Data needs to be of type 'zoo'")
  }
  NAMES = colnames(x)
  k = ncol(x)
  if (is.null(NAMES)) {
    NAMES = 1:k
  }
  if (is.null(alpha)) {
    alpha = seq(0, 1, 1/n_alpha)
  }
  alpha_ = Res = B = NULL
  for (i in 1:k) {
    MAE = NULL
    for (j in 1:length(alpha)) {
      z = embed(x, nlag+1)
      X = z[,-c(1:k)]
      y = z[,i]
      fit = glmnet::cv.glmnet(X, y, alpha=alpha[j], type.measure=loss, nfolds=nfolds)
      y_pred = predict(fit, s=fit$lambda.min, newx=X)
      MAE[j] = mean(abs(y - y_pred))
    }
    fit = glmnet::cv.glmnet(X, y, alpha=alpha[which(MAE==min(MAE))], type.measure=loss, nfolds=nfolds)
    y_pred = predict(fit, s=fit$lambda.min, newx=X)
    Res = cbind(Res, y - y_pred)
    b = predict(fit, type="coefficients", s=fit$lambda.min)[-1]
    B = rbind(B, b)
    alpha_[i] = alpha[which(MAE==min(MAE))]
  }
  Q = array(t(Res)%*%Res/nrow(Res), c(k, k, 1), dimnames=c(list(NAMES), list(NAMES), list(zoo::index(x)[nrow(x)])))
  results = list(B=B, Q=Q, alpha=alpha_)
}

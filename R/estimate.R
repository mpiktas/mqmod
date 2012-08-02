##' Estimate moving quantile effects
##'
##' Estimate moving quantile effects for given time series
##' @param x data, a time series object.
##' @param k number of lags to use for calculating quantiles, a number.
##' @param quant quantiles to include in the model, a vector of values between zero and 1.
##' @param xreg  additional regressors, not supported
##' @return mq object
##' @author Vaidotas Zemlys
mq <- function(x,k,quant=c(1,0.75,0.25,0),xreg=NULL) {
    
}

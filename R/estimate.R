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
    yX <- lapply(0:k,function(i)lag(x,-i))
    yX <- do.call("ts.intersect",yX)
    colnames(yX) <- c("x",1:k)
    X <- t(apply(yX[,-1],1,quantile,probs=quant))
    colnames(X) <- paste("q",quant,sep="_")
    res <- lm(x~.,data=data.frame(x=yX[,1],X))
    res$x <- x
    res$k <- k
    res$quant <- quant
    class(res) <- c("mq")
    res
}
##' Summarising moving quantile effect model fit
##'
##' \code{summary} method for class \code{mq}. Passes object to \code{\link{summary.lm}} function.
##' @param object an object of class \code{mq}, usually, a result of a call to \code{\link{mq}}
##' @param ... additional arguments to \code{\link{summary.lm}}
##' @return A \code{\link{summary.lm}} object.
##' @author Vaidotas Zemlys
summary.mq <- function(object,...) {
    class(object) <- "lm"
    summary.lm(object,...)
}

##' Forecast a moving quantile effects model
##'
##' Forecasts moving quantile effects model for specified number of periods ahead
##' @param object a moving quantile model, result of a call to \code{\link{mq}}
##' @param h Number of periods for forecasting
##' @return a list with the following elements:
##' \item{mean}{the forecast}
##' \item{x}{the original data series}
##' \item{full}{original data series combined with the forecast}
##' @author Vaidotas Zemlys
forecast.mq <- function(object,h) {
    fc <- numeric(h)
    xlag <- tail(object$x,object$k)
    class(object) <- "lm"
    row <- object$model[1,-1,drop=FALSE]
    for(i in 1:h) {
        row[1,] <- quantile(xlag,probs=object$quant)        
        fc[i] <- predict.lm(object,newdata=row)
        xlag <- c(xlag[-1],fc[i])
    }
    fc <- ts(fc,start=end(lag(object$x,-1)),frequency=frequency(object$x))
    list(mean=fc,x=object$x,full=ts(c(object$x,fc),start=start(object$x),frequency=frequency(object$x)))
}



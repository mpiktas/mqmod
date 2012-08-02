\name{mq}
\alias{mq}
\title{Estimate moving quantile effects}
\usage{
  mq(x, k, quant = c(1, 0.75, 0.25, 0), xreg = NULL)
}
\arguments{
  \item{x}{data, a time series object.}

  \item{k}{number of lags to use for calculating quantiles,
  a number.}

  \item{quant}{quantiles to include in the model, a vector
  of values between zero and 1.}

  \item{xreg}{additional regressors, not supported}
}
\value{
  mq object
}
\description{
  Estimate moving quantile effects for given time series
}
\author{
  Vaidotas Zemlys
}


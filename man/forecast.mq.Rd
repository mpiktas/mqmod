\name{forecast.mq}
\alias{forecast.mq}
\title{Forecast a moving quantile effects model}
\usage{
  forecast.mq(object, h)
}
\arguments{
  \item{object}{a moving quantile model, result of a call
  to \code{\link{mq}}}

  \item{h}{Number of periods for forecasting}
}
\value{
  a list with the following elements: \item{mean}{the
  forecast} \item{x}{the original data series}
  \item{full}{original data series combined with the
  forecast}
}
\description{
  Forecasts moving quantile effects model for specified
  number of periods ahead
}
\author{
  Vaidotas Zemlys
}


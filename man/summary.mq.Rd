\name{summary.mq}
\alias{summary.mq}
\title{Summarising moving quantile effect model fit}
\usage{
  summary.mq(object, ...)
}
\arguments{
  \item{object}{an object of class \code{mq}, usually, a
  result of a call to \code{\link{mq}}}

  \item{...}{additional arguments to
  \code{\link{summary.lm}}}
}
\value{
  A \code{\link{summary.lm}} object.
}
\description{
  \code{summary} method for class \code{mq}. Passes object
  to \code{\link{summary.lm}} function.
}
\author{
  Vaidotas Zemlys
}


% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shorts_S3.R
\name{coef.shorts_model}
\alias{coef.shorts_model}
\title{S3 method for extracting model parameters from \code{shorts_model} object}
\usage{
\method{coef}{shorts_model}(object, ...)
}
\arguments{
\item{object}{\code{shorts_model} object}

\item{...}{Extra arguments. Not used}
}
\description{
S3 method for extracting model parameters from \code{shorts_model} object
}
\examples{
split_times <- data.frame(
  distance = c(5, 10, 20, 30, 35),
  time = c(1.20, 1.96, 3.36, 4.71, 5.35)
)

# Simple model
simple_model <- with(
  split_times,
  model_using_splits(distance, time)
)

# unlist(simple_model$parameters)
coef(simple_model)
}

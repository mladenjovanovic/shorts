% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixed_model_using_splits.R
\name{mixed_model_split_times}
\alias{mixed_model_split_times}
\alias{mixed_model_using_splits}
\alias{mixed_model_using_splits_with_time_correction}
\alias{mixed_model_using_splits_with_corrections}
\title{Mixed Models Using Split Times}
\usage{
mixed_model_using_splits(
  data,
  distance,
  time,
  athlete,
  time_correction = 0,
  random = MSS + TAU ~ 1,
  LOOCV = FALSE,
  na.rm = FALSE,
  ...
)

mixed_model_using_splits_with_time_correction(
  data,
  distance,
  time,
  athlete,
  random = MSS + TAU ~ 1,
  LOOCV = FALSE,
  na.rm = FALSE,
  ...
)

mixed_model_using_splits_with_corrections(
  data,
  distance,
  time,
  athlete,
  random = MSS + TAU ~ 1,
  LOOCV = FALSE,
  na.rm = FALSE,
  ...
)
}
\arguments{
\item{data}{Data frame}

\item{distance}{Character string. Name of the column in \code{data}}

\item{time}{Character string. Name of the column in \code{data}}

\item{athlete}{Character string. Name of the column in \code{data}. Used as levels in the \code{\link[nlme]{nlme}}}

\item{time_correction}{Numeric vector. Used to correct for different starting techniques.  This correction is
done by adding \code{time_correction} to \code{time}. Default is 0. See more in Haugen et al. (2018)}

\item{random}{Formula forwarded to \code{\link[nlme]{nlme}} to set random effects. Default is \code{MSS + TAU ~ 1}}

\item{LOOCV}{Should Leave-one-out cross-validation be used to estimate model fit? Default is \code{FALSE}}

\item{na.rm}{Logical. Default is FALSE}

\item{...}{Forwarded to \code{\link[nlme]{nlme}} function}
}
\value{
List object with the following elements:
    \describe{
        \item{parameters}{List with two data frames: \code{fixed} and \code{random} containing the following
            estimated parameters: \code{MSS}, \code{TAU}, \code{time_correction}, \code{distance_correction},
            \code{MAC}, and \code{PMAX}}
        \item{model_fit}{List with the following components:
            \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
        \item{model}{Model returned by the \code{\link[nlme]{nlme}} function}
        \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{athlete}, \code{distance},
            \code{time}, and \code{pred_time} columns}
        }
}
\description{
These functions model the sprint split times using mono-exponential equation, where \code{time}
 is used as target or outcome variable, and \code{distance} as predictor. Function
 \code{\link{mixed_model_using_splits}} provides the simplest model with estimated \code{MSS} and \code{TAU}
 parameters. Time correction using heuristic rule of thumbs (e.g., adding 0.3s to split times) can be
 implemented using \code{time_correction} function parameter. Function
 \code{\link{mixed_model_using_splits_with_time_correction}}, besides estimating \code{MSS} and \code{TAU},
 estimates additional parameter \code{time_correction}.  Function \code{\link{mixed_model_using_splits_with_corrections}},
 besides estimating \code{MSS}, \code{TAU} and \code{time_correction}, estimates additional parameter
 \code{distance_correction}. For more information about these function please refer to accompanying vignettes in
 this package.
}
\examples{
data("split_times")

mixed_model <- mixed_model_using_splits(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete"
)

print(mixed_model)
coef(mixed_model)
plot(mixed_model)

mixed_model <- mixed_model_using_splits_with_time_correction(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete"
)

print(mixed_model)
coef(mixed_model)
plot(mixed_model)

mixed_model <- mixed_model_using_splits_with_corrections(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete"
)

print(mixed_model)
coef(mixed_model)
plot(mixed_model)
}
\references{
Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
        Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
        DOI: 10.1519/JSC.0b013e318226030b.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physio_timeplot.R
\name{physio_timeplot}
\alias{physio_timeplot}
\title{plots data from dm}
\usage{
physio_timeplot(
  data,
  plotmetric,
  plotdv,
  selection = NULL,
  contrast = NULL,
  smoother = 0.3,
  plot_error = F,
  title = NULL,
  outp = "analysis",
  h = 4,
  w = 6
)
}
\arguments{
\item{data}{data list from physio_merge}

\item{plotmetric}{The metric in data$dm to plot, eg DET or norm}

\item{plotdv}{The dv, eg HR or EA}

\item{selection}{A datatable with the columns and values that you want to use to select the data to plot}

\item{smoother}{parameter for loess smoothing. set to 0 for none}

\item{plot_error}{give error from smoothing}
}
\description{
plots data from dm
}

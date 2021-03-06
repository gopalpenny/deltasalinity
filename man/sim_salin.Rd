% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/salinity-model-functions.R
\name{sim_salin}
\alias{sim_salin}
\title{Simulate salinity for a timeseries of streamflow}
\usage{
sim_salin(Q_ts, v, salin_init = 100, salin_min = 100)
}
\arguments{
\item{Q_ts}{Timeseries of daily streamflow values}

\item{v}{Vector of length 4 containing log parameter values: \code{log(a), log(b), log(d), and log(C_d)}}

\item{salin_init}{Initial salinity for simulation}

\item{salin_min}{Minimum value of salinity}
}
\description{
Simulate salinity for a timeseries of streamflow
}
\examples{
streamflow_df <- ganges_streamflow[ganges_streamflow$date < "2000-01-01",]
# Output salinity in ppm
streamflow_df$S_ppm <- sim_salin(streamflow_df$Q_cumec, ganges_params$param)
head(streamflow_df)
}

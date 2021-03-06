% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/salinity-SLR.R
\name{get_ganges_SLR_ratios}
\alias{get_ganges_SLR_ratios}
\title{Get channel volume ratios}
\usage{
get_ganges_SLR_ratios(SLR_m, control_volume = "channel")
}
\arguments{
\item{SLR_m}{vector of sea level rise in meters}

\item{control_volume}{set to "channel" or "gw"}
}
\description{
Get channel volume ratios under sea level rise
}
\details{
This function estimates the change in control volume storage relative to
a baseline of 2010.
}
\examples{
library(ggplot2)
channel_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "channel")
ggplot(channel_ratios) +
  geom_line(aes(SLR_m, R_E, color = "R_E")) +
  geom_line(aes(SLR_m, R_V, color = "R_V")) + ylab("Ratio")

gw_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "gw")
ggplot(gw_ratios) +
  geom_line(aes(SLR_m, R_E, color = "R_E")) +
  geom_line(aes(SLR_m, R_V, color = "R_V")) + ylab("Ratio")
}
\keyword{internal}

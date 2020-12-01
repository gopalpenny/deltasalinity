# salinity_model_functions.R

#' Simulate salinity, re-initializing each year
#'
#' @param Q_df A \code{data.frame} containing \code{Q_cumec} and \code{year} columns. The rows must be in order of consecutive days.
#' @param v Vector of length 4 containing log parameter values: \code{log(a), log(b), log(d), and log(C_d)}
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Create new data.frame with streamflow input variables
#' streamflow_df <- ganges_streamflow
#'
#' # Simulate salinity each year
#' streamflow_df$S_ppm <- sim_salin_annual(ganges_streamflow, ganges_params$param)
#'
#' # Plot the output
#' ggplot(streamflow_df) + geom_line(aes(yday,S_ppm, color = group))
sim_salin_annual <- function(Q_df,v) {

  if (!("year" %in% names(Q_df))) {
    Q_df$year <- as.numeric(strftime(Q_df$date,"%Y"))
  }

  Q_split <- split(Q_df$Q_cumec,Q_df$year)

  if (!identical(Q_df$Q_cumec, as.numeric(do.call(c,Q_split)))) {
    stop("Ensure that Q_df is ordered so that each year is clustered together")
  }

  S_synth_df <- do.call(c,lapply(Q_split,sim_salin,v=v))
  S_synth <- as.numeric(S_synth_df)

  return(S_synth)
}

#' Simulate salinity for a timeseries of streamflow
#' @param Q_ts Timeseries of daily streamflow values
#' @param v Vector of length 4 containing log parameter values: \code{log(a), log(b), log(d), and log(C_d)}
#' @param salin_init Initial salinity for simulation
#' @param salin_min Minimum value of salinity
#' @export
#' @examples
#' streamflow_df <- ganges_streamflow[ganges_streamflow$date < "2000-01-01",]
#' # Output salinity in ppm
#' streamflow_df$S_ppm <- sim_salin(streamflow_df$Q_cumec, ganges_params$param)
#' head(streamflow_df)
sim_salin=function(Q_ts, v, salin_init = 100, salin_min=100) {
  #Initialize #exp to be positive
  Cobs_init <- salin_init
  parms=c(a=exp(v[1]),
          b=exp(v[2]),
          d=exp(v[3]),
          C_d=exp(v[4]))

  gradfun <- function(t,C,parms) {
    #see https://stackoverflow.com/questions/21557634/using-a-time-series-of-parameters-to-solve-ode-in-r
    Q <- Q_ts[pmax(1,ceiling(t))]
    with(as.list(c(parms,C,Q)),{
      list(max(-a*Q*C + b*exp(-d*Q)*(C_d-C),-(C-salin_min)), NULL)
    })
  }
  # gradfun(1,3000,parms)
  times=seq(1,length(Q_ts),by=1)
  salinity <- deSolve::ode(c(Cobs_init),times,gradfun,parms,method="rk4")[,2]

  #return SSE
  return(salinity)
}


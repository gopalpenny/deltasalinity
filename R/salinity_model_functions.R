# salinity_model_functions.R

#' Simulate salinity, re-initializing each year
#'
#' @param Q_df A \code{data.frame} containing \code{Q_cumec} and \code{year} columns
#' @param v Vector of length 4 containing log parameter values: \code{log(a), log(b), log(d), and log(C_d)}
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' salinity_results <- sim_salin_annual(ganges_streamflow, ganges_params$param)
#' salinity_results_joined <- ganges_streamflow %>%
#'   left_join(salinity_results[,c("date","S_ppm")], by = "date")
#' ggplot(salinity_results_joined) + geom_line(aes(yday,S_ppm, color = group))
sim_salin_annual <- function(Q_df,v) {
  S_synth_df <- do.call(c,lapply(split(Q_df$Q_cumec,Q_df$year),
                              sim_salin,v=v))
  S_synth <- as.numeric(S_synth_df)

  Q_output <- tibble::tibble(date = Q_df$date, Q_cumec = Q_df$Q_cumec, S_ppm = S_synth)
  return(Q_output)
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


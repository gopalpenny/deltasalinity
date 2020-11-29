
#' Prep synthetic streamflow timeseries
#'
#' Prepare synthetic salinity timeseries by combining observed streamflow
#' with synthetic streamflow from only part of the year
#' @param Q_ts Streamflow values
#' @param Q_obs_df contains date and all observed Q_cumec
#' @param v parameters of model.
#' @param date vector of dates for Q_ts
#' @param salin_min minimum salinity in ppm
sim_salin_prep <- function(Q_ts=NULL,pred_dates=NULL,Q_obs_df=NULL,salin_min=100) {

  # join synthetic streamflow with observed streamflow and smooth
  Q_df <- Q_obs_df %>% #rename(obs=Q_cumec) %>%
    left_join(tibble(pred=Q_ts,date=pred_dates),by="date") %>%
    mutate(type=ifelse(!is.na(pred),"pred","obs"),
           pred_temp=ifelse(!is.na(pred),pred,Q_cumec),
           year=as.integer(strftime(date,"%Y")),
           yearless_date=as.Date(paste("0000",strftime(date,"%m"),strftime(date,"%d"),sep="-")),
           pre_post=factor(yearless_date>"0000-04-01",levels=c(FALSE,TRUE),labels=c("pre","post"))) %>%
    filter(year >=1998,year %in% unique(as.integer(strftime(pred_dates,"%Y")))) %>%
    group_by(year) %>%
    mutate(pre_pred_date=min(date[type=="pred"]),
           pre_obs_date=max(date[type=="obs" & yearless_date < "0000-03-01"]),
           post_pred_date=max(date[type=="pred"]),
           post_obs_date=min(date[type=="obs" & yearless_date > "0000-05-01"])) %>%
    mutate(pre_pred_temp=ifelse(date==pre_pred_date,pred_temp,0),
           pre_obs_temp=ifelse(date==pre_obs_date,pred_temp,0),
           post_pred_temp=ifelse(date==post_pred_date,pred_temp,0),
           post_obs_temp=ifelse(date==post_obs_date,pred_temp,0)) %>%
    mutate(pre_pred=max(pre_pred_temp),
           pre_obs=max(pre_obs_temp),
           post_pred=max(post_pred_temp),
           post_obs=max(post_obs_temp)) %>%
    mutate(yday=as.integer(strftime(date,"%j")),
           scale=ifelse(yday >= 20 & yday <= 50 & type=="obs",(pre_pred/pre_obs-1)/30 * (yday-20) + 1,
                        ifelse(yday >= 150 & yday <= 182 & type=="obs",(1-post_pred/post_obs)/32 * (yday-150) + post_pred/post_obs,1)),
           pred_smooth=pred_temp*scale) %>%
    select(-(pre_pred_date:post_obs_temp)) %>% filter(yearless_date<="0000-08-31")

  # get synthetic salinity for each year

  return(Q_df)
}

#' Simulate salinity, re-initializing each year
#'
#' @param Q_df A \code{data.frame} containing \code{Q_cumec} and \code{year} columns
#' @param v Vector of length 4 containing log parameter values: \code{log(a), log(b), log(d), and log(C_d)}
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
#' @param salin_min Initial (and minimum) value of salinity
sim_salin=function(Q_ts,v,salin_min=100){
  #Initialize #exp to be positive
  Cobs_init <- salin_min
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


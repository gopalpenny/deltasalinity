# prep_synthetic_streamflow.R

#' Join partial timeseries of synthetic streamflow with observed streamflow
#'
#' Prepare synthetic salinity timeseries by combining observed streamflow
#' with synthetic streamflow from only part of the year
#' @param Q_ts Vector of streamflow values
#' @param Q_obs_df Data.frame that contains columns with date and observed Q_cumec
#' @param pred_dates vector of dates for Q_ts
#' @keywords internal
sim_salin_prep <- function(Q_ts=NULL,pred_dates=NULL,Q_obs_df=NULL) {

  # join synthetic streamflow with observed streamflow and smooth
  Q_df <- Q_obs_df %>% #rename(obs=Q_cumec) %>%
    dplyr::left_join(tibble(pred=Q_ts,date=pred_dates),by="date") %>%
    dplyr::mutate(type=ifelse(!is.na(pred),"pred","obs"),
           pred_temp=ifelse(!is.na(pred),pred,Q_cumec),
           year=as.integer(strftime(date,"%Y")),
           yearless_date=as.Date(paste("0000",strftime(date,"%m"),strftime(date,"%d"),sep="-")),
           pre_post=factor(yearless_date>"0000-04-01",levels=c(FALSE,TRUE),labels=c("pre","post"))) %>%
    dplyr::filter(year >=1998,year %in% unique(as.integer(strftime(pred_dates,"%Y")))) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(pre_pred_date=min(date[type=="pred"]),
           pre_obs_date=max(date[type=="obs" & yearless_date < "0000-03-01"]),
           post_pred_date=max(date[type=="pred"]),
           post_obs_date=min(date[type=="obs" & yearless_date > "0000-05-01"])) %>%
    dplyr::mutate(pre_pred_temp=ifelse(date==pre_pred_date,pred_temp,0),
           pre_obs_temp=ifelse(date==pre_obs_date,pred_temp,0),
           post_pred_temp=ifelse(date==post_pred_date,pred_temp,0),
           post_obs_temp=ifelse(date==post_obs_date,pred_temp,0)) %>%
    dplyr::mutate(pre_pred=max(pre_pred_temp),
           pre_obs=max(pre_obs_temp),
           post_pred=max(post_pred_temp),
           post_obs=max(post_obs_temp)) %>%
    dplyr::mutate(yday=as.integer(strftime(date,"%j")),
           scale=ifelse(yday >= 20 & yday <= 50 & type=="obs",(pre_pred/pre_obs-1)/30 * (yday-20) + 1,
                        ifelse(yday >= 150 & yday <= 182 & type=="obs",(1-post_pred/post_obs)/32 * (yday-150) + post_pred/post_obs,1)),
           pred_smooth=pred_temp*scale) %>%
    dplyr::select(-(pre_pred_date:post_obs_temp)) %>% dplyr::filter(yearless_date<="0000-08-31")

  # get synthetic salinity for each year

  return(Q_df)
}

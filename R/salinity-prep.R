# prep_synthetic_streamflow.R

#' Join partial timeseries of synthetic streamflow with observed streamflow
#'
#' Prepare synthetic salinity timeseries by combining observed streamflow
#' with synthetic streamflow from only part of the year
#' @param Q_ts Vector of streamflow values
#' @param Q_obs_df Data.frame that contains columns with date and observed Q_cumec
#' @param pred_dates vector of dates for Q_ts
#' @importFrom rlang .data
#' @keywords internal
sim_salin_prep <- function(Q_ts=NULL,pred_dates=NULL,Q_obs_df=NULL) {

  # join synthetic streamflow with observed streamflow and smooth
  Q_df <- Q_obs_df %>% #rename(obs=Q_cumec) %>%
    dplyr::left_join(tibble::tibble(pred=Q_ts,date=pred_dates),by="date") %>%
    dplyr::mutate(type=ifelse(!is.na(.data$pred),"pred","obs"),
           pred_temp=ifelse(!is.na(.data$pred),.data$pred,.data$Q_cumec),
           year=as.integer(strftime(.data$date,"%Y")),
           yearless_date=as.Date(paste("0000",strftime(.data$date,"%m"),strftime(.data$date,"%d"),sep="-")),
           pre_post=factor(.data$yearless_date>"0000-04-01",levels=c(FALSE,TRUE),labels=c("pre","post"))) %>%
    dplyr::filter(.data$year >=1998,.data$year %in% unique(as.integer(strftime(pred_dates,"%Y")))) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(pre_pred_date=min(.data$date[.data$type=="pred"]),
           pre_obs_date=max(.data$date[.data$type =="obs" & .data$yearless_date < "0000-03-01"]),
           post_pred_date=max(.data$date[.data$type =="pred"]),
           post_obs_date=min(.data$date[.data$type =="obs" & .data$yearless_date > "0000-05-01"])) %>%
    dplyr::mutate(pre_pred_temp=ifelse(.data$date ==.data$pre_pred_date,.data$pred_temp,0),
           pre_obs_temp=ifelse(.data$date ==.data$pre_obs_date, .data$pred_temp, 0),
           post_pred_temp=ifelse(.data$date == .data$post_pred_date, .data$pred_temp,0),
           post_obs_temp=ifelse(.data$date == .data$post_obs_date, .data$pred_temp,0)) %>%
    dplyr::mutate(pre_pred=max(.data$pre_pred_temp),
           pre_obs=max(.data$pre_obs_temp),
           post_pred=max(.data$post_pred_temp),
           post_obs=max(.data$post_obs_temp)) %>%
    dplyr::mutate(yday=as.integer(strftime(.data$date,"%j")),
           scale=ifelse(.data$yday >= 20 & .data$yday <= 50 & .data$type=="obs",(.data$pre_pred/.data$pre_obs-1)/30 * (.data$yday-20) + 1,
                        ifelse(.data$yday >= 150 & .data$yday <= 182 & .data$type=="obs",(1-.data$post_pred/.data$post_obs)/32 * (.data$yday-150) + .data$post_pred/.data$post_obs,1)),
           pred_smooth=.data$pred_temp*.data$scale) %>%
    dplyr::select(-(.data$pre_pred_date:.data$post_obs_temp)) %>% dplyr::filter(.data$yearless_date<="0000-08-31")

  # get synthetic salinity for each year

  return(Q_df)
}

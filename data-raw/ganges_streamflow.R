## code to prepare `ganges_streamflow` dataset goes here


# Load the data
Q_obs <- read_csv("./ignore/ganges_streamflow/Q_observed_filtered.csv")
Q_synth <- read_csv("./ignore/ganges_streamflow/synthetic_streamflow_quantiles_0_100.csv")

Q_treaty_hardinge_avg <- Q_obs %>% dplyr::mutate(yearless_date = as.Date(strftime(date,"0000-%m-%d"))) %>%
  dplyr::filter(date >= "1998-01-01") %>%
  dplyr::group_by(yearless_date) %>%
  dplyr::summarize(Q_cumec = mean(Q_cumec)) %>%
  dplyr::mutate(yday = as.numeric(strftime(yearless_date,"%j")),
                year = 1,
                group = "Treaty avg") %>%
  dplyr::filter(yearless_date <= "0000-05-31") %>%
  dplyr::mutate(date = as.Date(strftime(yearless_date,"1998-%m-%d"))) %>%
  select(-yearless_date)

# Prepare the data
Q_ts <- Q_synth %>% dplyr::pull(predOLSNO_Q050) # select the median quantile, Q050
Q_dates <- Q_synth %>% dplyr::pull(date)
Q_prepped <- sim_salin_prep(Q_ts, pred_dates=Q_dates, Q_obs_df=Q_obs) %>%
  dplyr::filter(year == 2006, date <= "2006-05-31") %>%
  dplyr::mutate(group = "2006 synthetic, no treaty")

ganges_streamflow <- Q_treaty_hardinge_avg %>%
  bind_rows(Q_prepped %>% dplyr::select(Q_cumec = pred_smooth,year,yday,group, date)) %>%
  dplyr::select(year, date, yday, Q_cumec, group)

# ggplot(streamflow_df) + geom_line(aes(yday,Q_cumec,color = group))

usethis::use_data(ganges_streamflow, overwrite = TRUE)

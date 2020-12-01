## code to prepare `ganges_streamflow` dataset goes here

library(readr)

# Load the data
Q_obs <- read_csv("./ignore/ganges_streamflow/Q_observed_filtered.csv")
Q_synth <- read_csv("./ignore/ganges_streamflow/synthetic_streamflow_quantiles_0_100.csv")

Q_treaty_hardinge_avg <- Q_obs %>%
  dplyr::mutate(yearless_date = as.Date(strftime(date,"0000-%m-%d"))) %>%
  dplyr::filter(date >= "1998-01-01") %>%
  dplyr::group_by(yearless_date) %>%
  dplyr::summarize(Q_cumec = mean(Q_cumec)) %>%
  dplyr::filter(yearless_date <= "0000-06-30", yearless_date != as.Date("0000-02-29")) %>%
  dplyr::mutate(date = as.Date(strftime(yearless_date,"1998-%m-%d")),
                yday = as.numeric(strftime(date,"%j")),
                year = 1,
                group = "Treaty avg")
# Prepare the data
Q_ts <- Q_synth %>% dplyr::pull(predOLSNO_Q050) # select the median quantile, Q050
Q_dates <- Q_synth %>% dplyr::pull(date)
Q_prepped <- sim_salin_prep(Q_ts, pred_dates=Q_dates, Q_obs_df=Q_obs) %>%
  dplyr::filter(year %in% c(2006), yearless_date <= "0000-06-30") %>%
  left_join(Q_treaty_hardinge_avg %>% select(yearless_date, Q_treaty_avg = Q_cumec), by = "yearless_date")#

# shift 2006 data to follow treaty average in June
shift_2006 <- Q_treaty_hardinge_avg$Q_cumec[Q_treaty_hardinge_avg$yearless_date == "0000-05-31"] -
  Q_prepped$Q_cumec[Q_prepped$date == "2006-05-31"]
Q_prepped <- Q_prepped %>%
  dplyr::mutate(group = "2006 synthetic, no treaty",
                Q_cumec = if_else(date > as.Date("2006-05-31"), Q_treaty_avg - shift_2006, pred_smooth))

ganges_streamflow <- Q_treaty_hardinge_avg %>%
  bind_rows(Q_prepped %>% dplyr::select(Q_cumec,year,yday,group, date)) %>%
  dplyr::select(year, date, yday, Q_cumec, group)

# ggplot(ganges_streamflow) + geom_line(aes(yday,Q_cumec,color = group))

usethis::use_data(ganges_streamflow, overwrite = TRUE)

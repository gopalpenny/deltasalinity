library(readr)
library(dplyr)

Q_obs_2011 <- read_csv("./ignore/ganges_streamflow/Q_observed_filtered.csv") %>%
  filter(date >= "2011-01-01", date<="2011-06-30")

# Get calibration data
treat_salinity_54cm <- Q_obs_2011 %>%
  bind_cols(S_ppm_model_baseline = sim_salin_annual(Q_obs_2011, v = ganges_params$param)) %>%
  mutate(S_ppm_SLR_calibrate = if_else(date == "2011-06-06",
                                       S_ppm_model_baseline * (dasgupta_slr67cm_pct_increase+1), as.numeric(NA)))

### Update calibration parameters a and b
v_ab_channel_slr54 <- update_ab_SLR(ganges_params$param, SLR_m = 0.54, "channel")
### Update calibration parameters a and b
v_ab_gw_slr54 <- update_ab_SLR(ganges_params$param, SLR_m = 0.54, "gw")

### Calibrate parameter d
# sea level rise of 54 cm
v_slr54_channel <- treat_salinity_54cm %>% rename(S_ppm = S_ppm_SLR_calibrate) %>%
  calibrate_salinity_model(v_ab_channel_slr54, control = list(trace = FALSE))
v_slr54_gw <- treat_salinity_54cm %>% rename(S_ppm = S_ppm_SLR_calibrate) %>%
  calibrate_salinity_model(v_ab_gw_slr54, control = list(trace = FALSE))

paste(v_slr54_channel,collapse=",")
# v_slr54_channel <- c(-12.6496881185107,-4.25847441165473,-6.64196860125567,10.4631033404715)
paste(v_slr54_gw,collapse=",")
# v_slr54_gw <-      c(-12.6090585920122,-4.25588413142161,-6.64803987054384,10.4631033404715)

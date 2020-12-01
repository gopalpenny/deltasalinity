library(dplyr)

# CHECK CALIBRATION

hydro_data <- ganges_streamflow %>%
  bind_cols(S_ppm = sim_salin_annual(ganges_streamflow, ganges_params$param))

# Add random error to salinity output
set.seed(100)
hydro_data$S_ppm_orig <- hydro_data$S_ppm
hydro_data$S_ppm <- hydro_data$S_ppm_orig * runif(nrow(hydro_data), min = 0.9, max = 1.1)
hydro_data$year <- as.numeric(strftime(hydro_data$date,"%Y"))

# Calibrate parameter "a"
v <- ganges_params$param
v[1] <- NA
v_calibrated <- suppressWarnings(calibrate_salinity_model(hydro_data, v, control = list(trace = F))) # warning expected for Nelder-Mead

# Check the percent difference
pct_diff1 <- (v_calibrated - ganges_params$param) / ganges_params$param * 100#
pct_diff1_output <- c(-0.0055, 0, 0, 0)

# Calibrate parameters "b" and "d"
v <- ganges_params$param
v[c(2,3)] <- NA
v_calibrated <- calibrate_salinity_model(hydro_data, v, control = list(trace = F))
# v_calibrated

# Check the percent difference
pct_diff2 <- (v_calibrated - ganges_params$param) / ganges_params$param * 100
pct_diff2_output <- c(0, -0.724, -0.2124, 0) # paste(round(pct_diff2,4),collapse = ", ")

test_that("check calibration using Nelder-Mead",{
  expect_equal(round(pct_diff1,4), pct_diff1_output)
  expect_equal(round(pct_diff2,4), pct_diff2_output)
})

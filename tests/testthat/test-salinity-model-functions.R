# test-salinity-model-functions.R

library(dplyr)

# Test sim_salin
streamflow_df <- ganges_streamflow[ganges_streamflow$date < "2000-01-01",]
streamflow_df$S_ppm <- sim_salin(streamflow_df$Q_cumec, ganges_params$param)
streamflow_test <- streamflow_df %>%
  filter(date %in% seq(as.Date("1998-05-01"),as.Date("1998-05-05"),by=1)) %>%
  dplyr::select(c("Q_cumec","S_ppm")) %>%
  dplyr::mutate(across(everything(),function(x) round(x,2)))
# ggp::print_data_frame_for_entry(streamflow_test)
streamflow_test_output <- tibble::tibble(
  Q_cumec=c(1364.99, 1364.48, 1347.19, 1311.43, 1238.01),
  S_ppm=c(3012.07, 3026.42, 3041.72, 3059.35, 3082.32))
test_that("Test for sim_salin using treaty data",{
  expect_equal(streamflow_test,streamflow_test_output)
})

# Simulate salinity annually
salinity_results <- sim_salin_annual(ganges_streamflow, calibrated_parameters$param)
salinity_results_may_treaty <- salinity_results %>%
  filter(date %in% seq(as.Date("1998-05-01"),as.Date("1998-05-05"),by=1)) %>%
  dplyr::select(c("Q_cumec","S_ppm")) %>%
  dplyr::mutate(across(everything(),function(x) round(x,2)))
salinity_results_may_2006 <- salinity_results %>%
  filter(date %in% seq(as.Date("2006-05-01"),as.Date("2006-05-05"),by=1)) %>%
  dplyr::select(c("Q_cumec","S_ppm")) %>%
  dplyr::mutate(across(everything(),function(x) round(x,2)))
# ggp::print_data_frame_for_entry(salinity_results_may_treaty)
salinity_results_may_treaty_output <- tibble::tibble(
  Q_cumec=c(1364.99, 1364.48, 1347.19, 1311.43, 1238.01),
  S_ppm=c(3012.07, 3026.42, 3041.72, 3059.35, 3082.32))
salinity_results_may_2006_output <- tibble::tibble(
  Q_cumec=c(741.01, 756.79, 748.68, 762.77, 758.12),
  S_ppm=c(7691.44, 7757.17, 7823.36, 7887.05, 7950.69))
testthat::test_that("Test sim_salin_annual for treaty and 2006",{
  expect_equal(salinity_results_may_treaty, salinity_results_may_treaty_output)
  expect_equal(salinity_results_may_2006, salinity_results_may_2006_output)
})


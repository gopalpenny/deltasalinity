library(dplyr)

channel_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "channel") %>%
  mutate(across(everything(), function(x) round(x,3)))
# channel_ratios[1:5,] %>% ggp::print_data_frame_for_entry()
channel_ratios_output <-
  data.frame(SLR_m=c(0, 0.05, 0.1, 0.15, 0.2),
             R_V=c(1, 1.009, 1.017, 1.026, 1.034),
             R_E=c(1, 1.004, 1.007, 1.011, 1.014))

gw_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "gw") %>%
  mutate(across(everything(), function(x) round(x,3)))
# gw_ratios[1:5,] %>% ggp::print_data_frame_for_entry()
gw_ratios_output <-
  data.frame(SLR_m=c(0, 0.05, 0.1, 0.15, 0.2),
             R_V=c(1, 1.005, 1.009, 1.014, 1.018),
             R_E=c(1, 1, 1, 1, 1))

test_that("get_ganges_SLR_channel_ratios",{
  expect_equal(dim(channel_ratios),c(21,3))
  expect_equal(channel_ratios[1:5,],channel_ratios_output)
  expect_equal(gw_ratios[1:5,],gw_ratios_output)
  expect_error(get_ganges_SLR_ratios(SLR_m = 0.5, "error"),regexp = "specify control_volume")
})



# TEST OVERALL PARAMETER UPDATES
# Load parameters for no sea level rise
v <- ganges_params$param

# Generate parameters for SLR of 0.25 and 0.5 m
# (including min, median, and maximum d for SLR of 0.5 m)
v_slr0 <- update_ganges_params_SLR(ganges_params$param, SLR_m = 0, "avg", d_probs = 0.5) %>% round(8)
v_slr25 <- update_ganges_params_SLR(ganges_params$param, SLR_m = 0.25, "avg", d_probs = 0.5) %>% round(8)
v_slr50 <- update_ganges_params_SLR(ganges_params$param, SLR_m = 0.50, "avg", d_probs = 0.5) %>% round(8)

v_slr25_output <- c(-12.59304515,-4.23102249,-6.32589499,10.46310334)
v_slr50_output <- c(-12.62427451,-4.25364259,-6.48691417,10.46310334)

test_that("update_ganges_params_SLR works for avg channel geometry",{
  expect_equal(v_slr0, v)
  expect_equal(v_slr25, v_slr25_output)
  expect_equal(v_slr50, v_slr50_output)
})

v_slr50_min_d <- update_ganges_params_SLR(ganges_params$param, SLR_m = 0.5, "avg", d_probs = 0) %>% round(8)
v_slr50_max_d <- update_ganges_params_SLR(ganges_params$param, SLR_m = 0.5, "avg", d_probs = 1) %>% round(8)

v_slr50_min_d_output <- c(-12.62427451,-4.25364259,-6.60559524,10.46310334)
v_slr50_max_d_output <- c(-12.62427451,-4.25364259,-6.38083581,10.46310334)

test_that("update_ganges_params_SLR works for d_probs = 0, 1",{
  expect_equal(v_slr50_min_d, v_slr50_min_d_output)
  expect_equal(v_slr50_max_d, v_slr50_max_d_output)
})

v_slr50_chan <- update_ganges_params_SLR(ganges_params$param, SLR_m = 0.5, "channel", d_probs = 0.5) %>% round(8)
v_slr50_gw <- update_ganges_params_SLR(ganges_params$param, SLR_m = 0.5, "gw", d_probs = 0.5) %>% round(8)

v_slr50_chan_output <- c(-12.64335608,-4.254911,-6.48691417,10.46310334)
v_slr50_gw_output <- c(-12.60555025,-4.25237578,-6.48691417,10.46310334)

test_that("update_ganges_params_SLR works for control_volume channel, gw",{
  expect_equal(v_slr50_chan, v_slr50_chan_output)
  expect_equal(v_slr50_gw, v_slr50_gw_output)
})
# paste0("c(",paste(v_slr50_max_d_gw,collapse=","),")")


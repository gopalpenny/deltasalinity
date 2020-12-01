library(dplyr)

channel_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "channel") %>%
  mutate(across(everything(), function(x) round(x,3)))
channel_ratios_output <-
  data.frame(SLR_m=c(0, 0.05, 0.1, 0.15, 0.2),
             R_V=c(1, 1.008, 1.016, 1.025, 1.033),
             R_E=c(1, 1.003, 1.007, 1.01, 1.014))

gw_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "gw") %>%
  mutate(across(everything(), function(x) round(x,3)))
gw_ratios_output <-
  data.frame(SLR_m=c(0, 0.05, 0.1, 0.15, 0.2),
             R_V=c(1, 1.004, 1.009, 1.013, 1.018),
             R_E=c(1, 1, 1, 1, 1))

test_that("get_ganges_SLR_channel_ratios",{
  expect_equal(dim(channel_ratios),c(21,3))
  expect_equal(channel_ratios[1:5,],channel_ratios_output)
  expect_equal(gw_ratios[1:5,],gw_ratios_output)
  expect_error(get_ganges_SLR_ratios(SLR_m = 0.5, "error"),regexp = "specify control_volume")
})

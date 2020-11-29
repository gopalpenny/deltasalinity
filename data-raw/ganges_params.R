## code to prepare `calibrated_parameters` dataset goes here
ganges_params <- readr::read_csv("~/Projects/GangesBrahmaputraProject/gbb_github/src/salinity_model/calibration_parameters.csv")
ganges_params <- data.frame(
  var=c("log(a)", "log(b)", "log(d)", "log(C_d)"),
  param=c(-12.5606246817699, -4.20745022117926, -6.18724300650039, 10.4631033404715))

usethis::use_data(ganges_params, overwrite = TRUE)

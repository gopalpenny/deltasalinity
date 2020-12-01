
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deltasalinity

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/gopalpenny/deltasalinity.svg?branch=main)](https://travis-ci.com/gopalpenny/deltasalinity)
[![codecov](https://codecov.io/gh/gopalpenny/deltasalinity/branch/main/graph/badge.svg?token=JQA0SJA46D)](https://codecov.io/gh/gopalpenny/deltasalinity)
<!-- badges: end -->

The goal of deltasalinity is to provide functions to calibrate and
implement a simple advection and first order exchange model of channel
salinity.

## Installation

You can install deltasalinity from
[github](https://github.com/gopalpenny/deltasalinity) with:

``` r
remotes::install_github("gopalpenny/deltasalinity")
```

## Simulating salinity under synthetic conditions

This is a basic example which shows you how simulate salinity at Khulna
in the Ganges delta using built-in streamflow data (`ganges_streamflow`)
and calibrated model parameters (`ganges_params`). The output salinity
(`S_ppm`) is salinity at Khulna in ppm.

``` r
library(deltasalinity)
library(ggplot2)

# Simulate salinity
salinity_results <- sim_salin_annual(ganges_streamflow, ganges_params$param)
salinity_results$yday <- as.numeric(strftime(salinity_results$date, "%j"))

# Join the simulated salinity output with the original input data
salinity_results_joined <- merge(ganges_streamflow, salinity_results[,c("date","S_ppm")], by = "date")

ggplot(salinity_results_joined) + geom_line(aes(yday,S_ppm, color = group))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

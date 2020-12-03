# salinity-SLR.R

#' Get channel volume ratios
#'
#' Get channel volume ratios under sea level rise
#' @param SLR_m vector of sea level rise in meters
#' @param control_volume set to "channel" or "gw"
#' @export
#' @keywords internal
#' @details
#' This function estimates the change in control volume storage relative to
#' a baseline of 2010.
#' @examples
#' library(ggplot2)
#' channel_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "channel")
#' ggplot(channel_ratios) +
#'   geom_line(aes(SLR_m, R_E, color = "R_E")) +
#'   geom_line(aes(SLR_m, R_V, color = "R_V")) + ylab("Ratio")
#'
#' gw_ratios <- get_ganges_SLR_ratios(SLR_m = seq(0,1,by = 0.05), "gw")
#' ggplot(gw_ratios) +
#'   geom_line(aes(SLR_m, R_E, color = "R_E")) +
#'   geom_line(aes(SLR_m, R_V, color = "R_V")) + ylab("Ratio")
get_ganges_SLR_ratios <- function(SLR_m, control_volume = "channel") {
  if (control_volume == "channel") {
    R_V = 0.1725 * SLR_m + 1 # calibrated for SLR relative to 2010
    R_E = 0.0718 * SLR_m + 1
  } else if (control_volume == "gw") {
    R_V = 0.0919 * SLR_m + 1
    R_E = rep(1, length(SLR_m))
  } else {
    stop("must specify control_volume as \"channel\" or \"gw\", not: ",control_volume)
  }
  return(data.frame(SLR_m = SLR_m, R_V = R_V, R_E = R_E))
}


#' Update a and b for sea level rise
#'
#' Update a and b parameters for sea level rise
#' @inheritParams get_ganges_SLR_ratios
#' @inheritParams sim_salin
#' @export
#' @keywords internal
#' @return
#' Returns the parameter vector v with updated a and b for a given sea level rise
#' and d set to NA.
#' @details
#' For a given sea level rise and initial v1 (parameters), this function
#' updates a and b as a2 = a1 / R_V and b_2 = b_1 R_E / R_V, where R_E
#' and R_V are determined from \code{get_ganges_SLR_ratios}. Because
#' v contains logged values, this is calculated as:
#' \itemize{
#' \item log(a2) = log(a1) - log(R_V)
#' \item log(b2) = log(b1) + log(R_E) - log(R_V)
#' }
#' @examples
#' library(ggplot2)
#'
#' # get updated v with sea level rise of 0.5 m
#' v_orig <- ganges_params$param
#' v_slr_chan <- update_ab_SLR(v_orig, SLR_m = 0.5, "channel")
#' v_slr_gw <- update_ab_SLR(v_orig, SLR_m = 0.5, "gw")
#'
#' # replace original value of d to create a dummy simulation
#' v_slr_chan[3] <- v_orig[3]
#' v_slr_gw[3] <- v_orig[3]
#'
#' # simulate salinity under original and new parameters
#' Q_df <- ganges_streamflow[ganges_streamflow$group == "Treaty avg",]
#' Q_df$S_ppm_orig <- sim_salin_annual(Q_df, v_orig)
#' Q_df$S_ppm_slr_chan <- sim_salin_annual(Q_df, v_slr_chan)
#' Q_df$S_ppm_slr_gw <- sim_salin_annual(Q_df, v_slr_gw)
#'
#' # plot the results
#' ggplot(Q_df) +
#'   geom_line(aes(date, S_ppm_orig, color = "orig")) +
#'   geom_line(aes(date, S_ppm_slr_chan, color = "slr 0.5 m (chan)")) +
#'   geom_line(aes(date, S_ppm_slr_gw, color = "slr 0.5 m (gw)")) +
#'   ylab("Salinity, ppm")
update_ab_SLR <- function(v, SLR_m, control_volume) {
  r <- get_ganges_SLR_ratios(SLR_m, control_volume)

  v_slr <- v
  v_slr[1] <- v[1] - log(r$R_V)
  v_slr[2] <- v[2] + log(r$R_E) - log(r$R_V)
  v_slr[3] <- NA

  return(v_slr)
}

#' Get parameter d for SLR
#'
#' Get parameter d for a given SLR and percentile
#' @inheritParams update_ab_SLR
#' @param d_probs Determines the quantile for d where 1 returns max(d), 0 returns min(d)
#' @details
#' Given the uncertainty of the parameter d under sea level rise, we specify this parameter
#' within a range from min(d) to max(d). The \code{d_probs} parameter determines which quantile
#' to extract within this range, with 0 being the lowest, 0.5 the median, and 1 the maximum.
#' This uniform distribution corresponds to the parameter d, not log(d) (although log(d) is returned).
#' Note that either SLR_m or d_probs can be a vector of length greater than 1, but not both.
#' @return
#' Returns the associated values as log(d).
#' @keywords internal
#' @examples
#' \dontrun{
#' get_d_SLR(SLR_m = 0.5)
#' }
get_d_SLR <- function(SLR_m, d_probs = 0.5) {
  if (length(SLR_m) > 1 & length(d_probs) > 1) {
    stop("Only SLR_m or d_probs can have length > 1")
  }
  SLR_cm <- SLR_m * 100
  d_slope_slr_cm <- data.frame(d_0=0.00205548590985469,
                               d_slope_max=-7.23558326670253e-06,
                               d_slope_min=-1.40541641857772e-05)
  d_min <- d_slope_slr_cm$d_0 + d_slope_slr_cm$d_slope_min * SLR_cm
  d_max <- d_slope_slr_cm$d_0 + d_slope_slr_cm$d_slope_max * SLR_cm
  d_quantile <- d_probs * d_max + (1 - d_probs) * d_min

  return(log(d_quantile))
}

#' Adjust Ganges salinity model to sea level rise
#'
#' Adjust Ganges salinity model at Khulna to sea level rise
#' @inheritParams update_ab_SLR
#' @inheritParams get_d_SLR
#' @details
#' This function takes a vector of logged parameter values as inputs c(log(a), log(b), log(d), log(C_d))
#' and adjusts the parameters for a given sea level rise in meters, relative to 2010. Parameters a and b
#' require specifying the control volume as "channel", "gw", or "avg", the last (avg) being the mean
#' of the first two. The resulting parameters that are returned are log transformed, but the average
#' (if taken) is calculated on the untransformed parameters.
#' @return
#' Returns a vector of logged parameters \code{c(log(a), log(b), log(d), log(C_d))}, calibrated to
#' future sea level rise (relative to 2010).
#' @export
#' @examples
#' # Load parameters for no sea level rise
#' v <- ganges_params$param
#'
#' # Generate parameters for SLR of 0.25 and 0.5 m
#' # (including min, median, and maximum d for SLR of 0.5 m)
#' v_slr25 <- adjust_ganges_SLR(ganges_params$param, SLR_m = 0.25, "avg", d_probs = 0.5)
#' v_slr50 <- adjust_ganges_SLR(ganges_params$param, SLR_m = 0.50, "avg", d_probs = 0.5)
#' v_slr50_mind <- adjust_ganges_SLR(ganges_params$param, SLR_m = 0.5, "avg", d_probs = 0)
#' v_slr50_maxd <- adjust_ganges_SLR(ganges_params$param, SLR_m = 0.5, "avg", d_probs = 1)
#'
#' # Simulate salnity for each of the parameter sets
#' results_df <- ganges_streamflow
#' results_df$S_ppm_current <- sim_salin_annual(results_df, v)
#' results_df$S_ppm_SLR25 <- sim_salin_annual(results_df, v_slr25)
#' results_df$S_ppm_SLR50 <- sim_salin_annual(results_df, v_slr50)
#' results_df$S_ppm_SLR50_mind <- sim_salin_annual(results_df, v_slr50_mind)
#' results_df$S_ppm_SLR50_maxd <- sim_salin_annual(results_df, v_slr50_maxd)
#'
#' # Plot the results
#' library(ggplot2)
#' ggplot(results_df) +
#'   geom_line(aes(yday, S_ppm_current, color = "current", linetype = "median d")) +
#'   geom_line(aes(yday, S_ppm_SLR25, color = "SLR 25 cm", linetype = "median d")) +
#'   geom_line(aes(yday, S_ppm_SLR50, color = "SLR 50 cm", linetype = "median d")) +
#'   geom_line(aes(yday, S_ppm_SLR50_mind, color = "SLR 50 cm", linetype = "min d")) +
#'   geom_line(aes(yday, S_ppm_SLR50_maxd, color = "SLR 50 cm", linetype = "max d")) +
#'   scale_linetype_manual(values = c("dotted","solid","dashed")) +
#'   facet_wrap(~group)
adjust_ganges_SLR <- function(v, SLR_m, control_volume, d_probs) {
  if (control_volume == "avg") {
    v_ab_channel <- update_ab_SLR(v, SLR_m, "channel")
    v_ab_gw <- update_ab_SLR(v, SLR_m, "gw")
    v_slr <- log( (exp(v_ab_channel) + exp(v_ab_gw))/2 )
  } else {
    v_slr <- update_ab_SLR(v, SLR_m, control_volume)
  }
  v_slr[3] <- get_d_SLR(SLR_m, d_probs = d_probs)
  return(v_slr)
}

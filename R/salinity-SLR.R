# salinity-SLR.R

#' Get channel volume ratios
#'
#' Get channel volume ratios under sea level rise
#' @param SLR_m vector of sea level rise in meters
#' @param control_volume set to "channel" or "gw"
#' @export
#' @keywords internal
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
    R_V = 0.1647 * SLR_m + 1
    R_E = 0.0694 * SLR_m + 1
  } else if (control_volume == "gw") {
    R_V = 0.08875 * SLR_m + 1
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

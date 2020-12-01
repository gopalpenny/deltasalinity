# salinity-SLR.R

#' Get channel volume ratios
#'
#' Get channel volume ratios under sea level rise
#' @param SLR_m vector of sea level rise in meters
#' @export
#' @keywords internal
#' @examples
#' channel_ratios <- get_ganges_SLR_channel_ratios(SLR_m = seq(0,1,by = 0.05))
#' ggplot(channel_ratios) +
#'   geom_line(aes(SLR_m, R_E, color = "R_E")) +
#'   geom_line(aes(SLR_m, R_V, color = "R_V")) + ylab("Ratio")
get_ganges_SLR_channel_ratios <- function(SLR_m) {
  R_V = 0.1647 * SLR_m + 1
  R_E = 0.0694 * SLR_m + 1
  return(data.frame(SLR_m = SLR_m, R_V = R_V, R_E = R_E))
}

#' Get groundwater volume ratios
#'
#' Get groundwater-connected volume ratios under sea level rise
#' @param SLR_m vector of sea level rise in meters
#' @export
#' @keywords internal
#' @examples
#' gw_ratios <- get_ganges_SLR_gw_ratios(SLR_m = seq(0,1,by = 0.05))
#' ggplot(gw_ratios) +
#'   geom_line(aes(SLR_m, R_E, color = "R_E")) +
#'   geom_line(aes(SLR_m, R_V, color = "R_V")) + ylab("Ratio")
get_ganges_SLR_gw_ratios <- function(SLR_m) {
  R_V = 0.08875 * SLR_m + 1
  R_E = rep(1, length(SLR_m))
  return(data.frame(SLR_m = SLR_m, R_V = R_V, R_E = R_E))
}

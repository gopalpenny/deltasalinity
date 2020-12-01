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

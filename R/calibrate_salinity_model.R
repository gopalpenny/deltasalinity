#' Optmize parameters
#' @param v logged parameter as a vector: log(a), log(b), log(d), log(C_d)
#' @param hydro_data data.frame containing \code{S_ppm} and \code{Q_cumec} columns for single year
#' @param sse logical: if \code{TRUE} return SSE, otherwise return vector of salinity
#' @param salin_init initial salinity. If set to NULL, function will preferentially take 1st salinity observation
#'   in hydro_data or salin_min
#' @param salin_min minimum salinity, ie river salinity inflow to control volume
#' @keywords internal
#' @details
#' This function returns the sum of squared error for \code{sim_salin()} using \code{hydro_data$S_ppm}
#' as a benchmark. Note that any elements of \code{hydro_data$S_ppm} that are \code{NA} will be ignored in
#' the calibration, meaning that it's possible to calibrate on a subset of dates by setting observed
#' salinity (\code{hydro_data$S_ppm}) to \code{NA} except on those dates where the model should be calibrated.
#' @examples
#' \dontrun{
#' hydro_data <- ganges_streamflow[ganges_streamflow$date < "2000-01-01",]
#' # Output salinity in ppm
#' hydro_data$S_ppm_model <- sim_salin(hydro_data$Q_cumec, ganges_params$param)
#' hydro_data$S_ppm <- hydro_data$S_ppm_model * runif(nrow(hydro_data), min = 0.9, max = 1.1)
#' v <- ganges_params$param
#' sse_sim_salin(v, hydro_data, sse = TRUE)
#' }
sse_sim_salin=function(v, hydro_data, sse = TRUE, salin_init = NULL, salin_min = 100){
  Cobs <- hydro_data$S_ppm
  Q_ts <- hydro_data$Q_cumec
  Cobs_init <- dplyr::case_when(
    !is.null(salin_init) ~ max(c(salin_init,-Inf)),
    !is.na(Cobs[1]) ~ Cobs[1],
    TRUE ~ salin_min)

  # Run sim_salin
  Cvect <- sim_salin(Q_ts, v, salin_init = Cobs_init, salin_min = salin_min)

  #return SSE
  if(sse) return(ifelse(sum(!is.na(Cvect))<=1,Inf,sum((Cvect-Cobs)^2,na.rm = T)))
  return(data.frame(Cvect))
}

#' Wrapper around sse
#' @param v_calibrate These values replace NA values in v
#' @param v Model parameters as log(a), lob(b), log(d), log(C_d). Set to NA those which
#'   should be replaced by \code{v_calibrate}
#' @param hydro_data data.frame containing \code{S_ppm}, \code{Q_cumec}, and \code{year} columns
#' @param sse logical, if TRUE will return SSE. Otherwise, returns full data.frame of results
#' @export
#' @details
#' This functions calculates SSE (sum of squared error), comparing modeled salinity output to the
#' calibration data (\code{hydro_data$S_ppm}). Salinity is modeled using daily streamflow
#' (\code{hydro_data$Q_cumec}) and the parameters \code{v} -- and those values of \code{v} that are
#' NA are replaced by values in \code{v_calibrate}.
#' @examples
#' hydro_data_orig <- ganges_streamflow
#' v <- ganges_params$param
#' # Output salinity in ppm
#' hydro_data <- sim_salin_annual(hydro_data_orig, v)
#'
#' # add random error to salinity output
#' set.seed(100)
#' hydro_data$S_ppm_orig <- hydro_data$S_ppm
#' hydro_data$S_ppm <- hydro_data$S_ppm_orig * runif(nrow(hydro_data), min = 0.9, max = 1.1)
#' hydro_data$year <- as.numeric(strftime(hydro_data$date,"%Y"))
#'
#' sse_all <- sse_wrapper(NULL, v, hydro_data)
#' sse_all
#'
#' hydro_data$Cvect <- sse_wrapper(NULL, v, hydro_data, FALSE)$Cvect
#' sse_all_manual <- sum((hydro_data$S_ppm - hydro_data$Cvect)^2)
#' sse_all_manual
#'
#' # Testing specific values
#' sse_wrapper(NULL, c(-12,v[2:4]), hydro_data)
sse_wrapper <- function(v_calibrate, v, hydro_data, sse=TRUE) {

  # replace NA values in v with calibrationp arameter values
  v[is.na(v)] <- v_calibrate

  hydro_data_split <- split(hydro_data,f=hydro_data$year)
  if(sse) {
    return(sum(sapply(hydro_data_split,sse_sim_salin,v=v,sse=sse)))
  } else {
    return(do.call(rbind,lapply(hydro_data_split,sse_sim_salin,v=v,sse=sse)))
  }
}

#' Calibrate salinity model to data
#'
#' Calibrate some or all salinity model parameters a, b, d, and C_d using salinity data.
#'
#' @param v logged parameters as a vector: log(a), log(b), log(d), log(C_d). Use NA for calibration params
#' @param hydro_data data.frame containing \code{S_ppm}, \code{Q_cumec}, and \code{year} columns
#' @param method "Nelder-Mead" or method supplied to \code{stats::optim} details below
#' @param control control parameters supplied to \code{stats::optim} for the \code{SANN} optimization
#' @export
#' @details
#' This function calibrates the parameters in \code{v} set to NA using streamflow and salinity in \code{hydro_data}.
#' The calibration is done by minimizing the sum of squared error of the modeled salinity with observed salinity.
#' The minimization is performed using \code{stats::optim} function with simulated annealing.
#'
#' Note that any values of \code{hydro_data$S_ppm} that are set to NA will be ignored in the calibration.
#' In other words, it's possible to calibrate on a subset of dates within this data.frame.
#'
#' The \code{method} input can be set to "auto", in which case "Nelder-Mead" is used for multi-variate
#' optimization and for univariate optimization ("Brent" has given poor results). This can also be set
#' to any of the \code{method}
#' options allowed by \code{stats::optim} -- see \code{?optim} for details.
#' @return
#' The function returns a list containing $v, all parameters including calibrated ones and
#' $optimization_outputs, which contains outputs from the optimization.
#' @examples
#' library(deltasalinity)
#' hydro_data <- sim_salin_annual(ganges_streamflow, ganges_params$param)
#'
#' # Add random error to salinity output
#' set.seed(100)
#' hydro_data$S_ppm_orig <- hydro_data$S_ppm
#' hydro_data$S_ppm <- hydro_data$S_ppm_orig * runif(nrow(hydro_data), min = 0.9, max = 1.1)
#' hydro_data$year <- as.numeric(strftime(hydro_data$date,"%Y"))
#'
#' # Calibrate parameter "a"
#' \dontrun{
#' # Nelder-Mead produces a warning with only 1 optimization parameter. Still seems to work though.
#' v <- ganges_params$param
#' v[1] <- NA
#' v_calibrated <- calibrate_salinity_model(hydro_data, v) # warning expected for Nelder-Mead
#'
#' # Check the percent difference
#' (v_calibrated$v - ganges_params$param) / ganges_params$param * 100
#' }
#'
#' # Calibrate parameters "b" and "d"
#' v <- ganges_params$param
#' v[c(2,3)] <- NA
#' v_calibrated <- calibrate_salinity_model(hydro_data, v)
#' v_calibrated$v
#'
#' # Check the percent difference
#' (v_calibrated$v - ganges_params$param) / ganges_params$param * 100
calibrate_salinity_model <- function(hydro_data, v, method = "Nelder-Mead", control = list(trace = T)) {
  # lower and upper bounds for optim
  lower <- -Inf
  upper <- Inf
  # Brent, L-BFGS-B, and CG all seem to produce poor results
  # if (method == "auto") {
  #   if (sum(is.na(v)) == 1) {
  #     method <- "L-BFGS-B"
  #     lower <- -1e10
  #     upper <- 1e10
  #   } else {
  #     method <- "Nelder-Mead"
  #   }
  # }

  cat("Calibrating using",method,"method...\n")

  v_init <- log(c(.00001,.001,.0001,10000))
  op=optim(par=v_init[is.na(v)], # par=log(c(.0000000001,.08,.0001,1000)),
           fn = sse_wrapper,
           hydro_data = hydro_data,
           v = v,
           lower = lower, upper = upper,
           method = method,
           control = control)
  v_calibrated <- v
  v_calibrated[is.na(v)] <- op$par
  return(list(v = v_calibrated, optimization_outputs = op))
}


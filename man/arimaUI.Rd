#' Gadget allowing to choose easily the parameter of an ARIMA model.
#' Some help to the decision making process is displayed as wikipedia link
#'
#' @param ts.ar A time series
#' @return The function open a viewer panel. It allows to choose the arima #'parameters. The value return is a list with the arima parameter and the #'coefficients of a fit of the time serie.
#'
#' @example 
#' sim.ts <- arima.sim(
#' n = 500, 
#' model = list(
#'   order = c(1, 0, 2), 
#'   ar = 0.8
#' , ma = c(0.2, 0.5)
#' ))
#'
#' list.res <- arimaUI(sim.ts)
#' #' # Comparison with auto.arima from the forecast package:
#' #' 
#' library(forecast)
#' auto.arima(sim.ts)


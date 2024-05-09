#' Re-sample melting curve data
#'
#' @param curve_data raw data
#' @param group group
#' @param from start of temperature
#' @param to end of temperature
#' @param by step
#' @param variable  c("fluorescence", "derivative")
#' @param method c("linear", "constant", "nearest", "spline", "pchip", "cubic")
#'
#' @return a tibble
#' @export
curve_resample = function(curve_data,
                           group = "well_position",
                           from = 70,
                           to = 90,
                           by = 0.1,
                           variable = c("fluorescence", "derivative"),
                           method = c("linear", "constant", "nearest", "spline", "pchip", "cubic")){
  variable = match.arg(variable)
  method = match.arg(method)
  df = curve_data |>
    dplyr::select(dplyr::all_of(c(group, "temperature", variable))) |>
    dplyr::select(dplyr::all_of(c(group, "temperature", variable))) |>
    tidyr::nest(data = c("temperature", variable))
  data = df$data
  new_data = lapply(seq_along(data), function(i){
    curve_interp1(data[[i]], from, to, by, variable, method)
  })
  new_data = lapply(data, curve_interp1, from = from, to = to, by = by, variable = variable, method = method)
  df$data = new_data
  df |> tidyr::unnest(cols = "data")
  df |> tidyr::unnest(cols = "data")
}

curve_interp1 = function(data, from, to, by, variable, method){
  x_new = seq(from, to, by = by)
  y_new = vector("numeric", length(x_new))
  if (method %in% c("linear", "nearest", "pchip", "cubic", "spline")) {
    y_new = signal::interp1(data[["temperature"]], data[[variable]], x_new, method = method)
  } else {
    y_new = pracma::interp1(data[["temperature"]], data[[variable]], x_new, method = method)
  }
  tbl = tibble::tibble(x = x_new, y = y_new) |>
    dplyr::filter(!is.na(.data$y))
  colnames(tbl) = c("temperature", variable)
  return(tbl)
}

#' Reconstruct signal with sinc method
#'
#' @param xx a vector of original independent value
#' @param yy a vector of original dependent value at xx
#' @param xi a vector of target independent variable values for reconstruction
#'
#' @return a vector
#' @export
#'
#' @examples
#'   xx = seq(1, 5, by = 0.5)
#'   yy = sin(xx)
#'   plot(xx, yy)
#'   xi = seq(1, 5, by = 0.1)
#'   yi = sinc_reconstruction(xx, yy, xi)
#'   plot(xi, yi)
sinc_reconstruction <- function(xx, yy, xi) {

  sinc <- function(x) {ifelse(x == 0, 1, sin(pi*x)/(pi*x))}

  yi <- sapply(xi, function(x_i) {
    sum(yy * sinc(x_i - xx))
  })

  return(yi)
}


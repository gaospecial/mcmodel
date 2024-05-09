#' Detect peaks in Melting Curve and return TM values
#'
#' @param mc a melting curve object
#' @param well_position if NULL return all wells
#' @inheritParams pracma::findpeaks
#'
#' @return a tibble
#' @export
#' @name curve-signal-process
#' @rdname curve-signal-process
mc_get_tm = function(mc,
                     npeaks = 3,
                     threshold = 0,
                     sortstr = TRUE,
                     zero = "+",
                     well_position = NULL) {
  if (inherits(mc, "MeltingCurve")) tbl = mc2tbl(mc)
  if (inherits(mc, "tbl")) tbl = mc
  if (!is.null(well_position)) {
    tbl = dplyr::filter(.data$well_position %in% well_position)
  }
  tbl_nested = mc_nest_by_well_position(tbl)
  result = lapply(1:nrow(tbl_nested), function(i){
    data = tbl_nested[["data"]][[i]]
    peak = detect_tm(data,
                     npeaks = npeaks,
                     threshold = threshold,
                     zero = zero,
                     sortstr = sortstr) |>
      dplyr::mutate(well_position = tbl_nested$well_position[[i]], .before = 1)
    return(peak)
  }) |>
    dplyr::bind_rows()
  return(result)
}

mc_nest_by_well_position = function(tbl){
  tbl |>
    dplyr::select(dplyr::all_of(c("well_position","temperature","derivative"))) |>
    dplyr::filter(!is.na(.data$derivative)) |>
    tidyr::nest(data = c("temperature","derivative"))
}

#' Remove trend in a melting curve
#'
#' @return a processed Melting Curve object
#' @export
#'
#' @rdname curve-signal-process
mc_rm_trend = function(mc){

}

#' @param data a tibble with `temperature` and `derivative` values for melting curve
#' @param ... will pass to `pracma::findpeaks()`
#' @seealso [pracma::findpeaks()]
#' @rdname curve-signal-process
detect_tm = function(data, ...){
  temperature = data[["temperature"]]
  derivative = data[["derivative"]]
  peak = pracma::findpeaks(derivative, ...)
  colnames(peak) = c("peak_height","peak_position","peak_start","peak_end")
  peak = dplyr::as_tibble(peak) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(2:4, ~ temperature[[.x]]))
  return(peak)
}

#' @title Remove the baseline of melting curve
#'
#' @param mc a MeltingCurve object
#' @param method c("spline","polyfit")
#' @param degree degree of freedom
#' @rdname curve-signal-process
mc_baseline = function(mc, method = c("spline","polyfit"), degree = 3){
  method = match.arg(method)
  if (!inherits(mc, "MeltingCurve")) stop("The first argument should be a MeltingCurve class object.")
  tbl = getData(mc)
  tbl_nested = mc_nest_by_well_position(tbl)
  l = tbl_nested$data
  tbl_nested$data = lapply(tbl_nested$data, .mc_baseline, method = method, degree = degree)
  mc@data = tidyr::unnest(tbl_nested, cols = "data")
  return(mc)
}


.mc_baseline = function(data, method, degree){
  x = data[["temperature"]]
  y = data[["derivative"]]
  baseline = vector("numeric", length(x))

  # smooth.spline
  if (method == "spline") {
    fit = stats::smooth.spline(x, y, df = degree)
    baseline = stats::predict(fit, x)$y
  }

  # smooth polyfit
  if (method == "polyfit") {
    fit = stats::lm(y ~ stats::poly(x, degree))
    baseline = stats::predict(fit, list(x = x))
  }
  corrected = y - baseline
  data$derivative = corrected
  return(data)
}


#' @title Methods for QuantStudioRaw class object
#'
#' @description
#'
#' **Input**
#'
#' The QuantStudioRaw object is a S3 class object by `read_quantstudio()` function.
#'
#' **Output**
#'
#' * `qs_plot()` - plot object
#' * `print()` - Print user-friendly information
#'
#' @name QuantStudioRaw-class
#' @rdname quantstudioraw-class
#' @docType class
#' @md
NULL

#' @param x QuantStudioRaw class object
#'
#' @param well_position specify the well position. If NULL, all wells are included.
#'
#' @rdname quantstudioraw-class
#' @export
qs_plot = function(x, well_position = NULL){
  if (!inherits(x, "QuantStudioRaw")) stop("x must be a 'QuantStudioRaw' object.")
  amplification_data = get_quantstudio_amplication(x)
  meltingcurve_data = get_quantstudio_melting_curve(x)
  if (!is.null(well_position)) {
    amplification_data = dplyr::filter(amplification_data, .data$well %in% pos2well(well_position))
    meltingcurve_data = dplyr::filter(meltingcurve_data, .data$well_position %in% !!well_position)
  }

  a1 = plot_quantstudio_amplification_curve(amplification_data, y = "rn") + ggplot2::labs(title = "Amplification")
  a2 = plot_quantstudio_amplification_curve(amplification_data, y = "delta_rn") + ggplot2::labs(title = "Amplification")
  m1 = plot_quantstudio_melting_curve(meltingcurve_data, y = "fluorescence") + ggplot2::labs(title = "Melting curve")
  m2 = plot_quantstudio_melting_curve(meltingcurve_data, y = "derivative") + ggplot2::labs(title = "Melting curve")

  aplot::plot_list(a1, a2, m1, m2, ncol = 2)
}

pos2well = function(position){
  well = 1:384
  names(well) = plate384() |> dplyr::pull('well_position')
  well[position] |> as.character()
}

#' @param x a QuantStudioRaw object
#' @inheritDotParams print
#' @method print QuantStudioRaw
#' @name print
#' @rdname quantstudioraw-class
#' @docType methods
#' @export
print.QuantStudioRaw = function(x, ...){
  cat("An object of class 'QuantStudioRaw':\n")
  cat("   Slots: ", paste0(names(x), collapse = ", "), ";\n", sep = "")
}

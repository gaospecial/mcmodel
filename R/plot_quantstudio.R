#' Plot real-time PCR melting/amplification curve
#'
#' @name quantstudio-plot
#'
#' @param data plot data
#' @param y mapping of y
#' @param show_tm if TRUE will plot the TM on melting curve
#' @param tm_nums how many TM, default is 1. But can be 2 if have two PCR products.
#'
#' @return a ggplot object
#' @export
plot_quantstudio_melting_curve = function(data,
                                          y = c("derivative","fluorescence"),
                                          show_tm = FALSE,
                                          tm_nums = 1){
  y = match.arg(y)
  if (show_tm) y = "derivative"
  if (!y %in% colnames(data)) stop(paste("Column", y, "not found in data."))
  p = data |>
    ggplot2::ggplot(ggplot2::aes(.data[["temperature"]],
                    .data[[y]],
                    color = .data[["well_position"]])) +
    ggplot2::geom_line(show.legend = FALSE)
  if (show_tm) {
    peak = mc_get_tm(data, npeaks = tm_nums)
    p = p +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = .data$peak_position,
                     color = .data$well_position),
        lty = "dashed",
        alpha = 0.8,
        data = peak,
        show.legend = FALSE) +
      ggrepel::geom_text_repel(
        mapping = ggplot2::aes(.data$peak_position,
                      .data$peak_height,
                      color = .data$well_position,
                      label = .data$peak_position),
        show.legend = FALSE,
        data = peak
      )
    return(p)
  } else {
    return(p)
  }
}

#' @rdname quantstudio-plot
#' @export
plot_quantstudio_amplification_curve = function(data, y = c("rn", "delta_rn")){
  y = match.arg(y)
  data |>
    ggplot2::ggplot(ggplot2::aes_string("cycle", y, color = "well")) +
    ggplot2::geom_line(show.legend = FALSE)
}

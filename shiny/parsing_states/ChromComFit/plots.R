#' Simple theme for plotting
simple_theme_grid <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "grey90"),
    panel.grid.minor = ggplot2::element_line(colour = "grey95"),
    axis.line = ggplot2::element_line(colour = "black")
  )





#' Plot panel with time lines
#'
#' @param m Pivoted data frame
#' @param single If true, no facets are applied
#' @param xmin Lower limit on x-axis
#' @param xmax Upper limit on x-axis
#'
#' @import ggplot2
timeline_panel <- function(m, single = FALSE, xmin = as.numeric(NA), xmax = as.numeric(NA)) {
  # Binding variables from non-standard evaluation locally
  Time <- Count <- Colour <- prop <- NULL

  c_palette <- STATE_COLOUR$colour
  ggplot(m, aes(x = Time, y = prop)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_line(aes(colour = Colour), size = 1.5) +
    scale_colour_manual(values = c_palette) +
    scale_fill_manual(values = c_palette) +
    theme(legend.position = "none") +
    xlim(xmin, xmax) +
    labs(x = "Time (min)", y = "Proportion") +
    geom_vline(xintercept = 0, color = "grey20", linetype = 1) +
    if(!single) facet_grid(X ~ Y)
}



#' Plot time lines
#'
#' @param chr A \code{chromcom} object with data
#' @param k Smoothing half-window size. If zero, no smoothing.
#' @param expdata Experimental data to add to the plot (another \code{chromcom} object)
#' @param title Title of the plot
#' @param title_size Font size for the title
#' @param withpars Title will be replaced with model parameters
#' @param time_limits A 2-element vector with time limits
#' @param with_ci Logical, whether to plot CI or not
#' @param ... Other parameters passed to \code{\link{timeline_panel}}
#'
#' @import ggplot2
#' @export
plot_timelines <- function(chr, k = 5, expdata = NULL, title = NULL, title_size = 10,
                          withpars = FALSE, time_limits = c(-50, 30), with_ci = FALSE, ...) {
  # Binding variables from non-standard evaluation locally
  Colour <- conf_lo <- conf_up <-  NULL

  m <- pivot_timelines(chr, k = 0, with_ci = FALSE)
  if(!is.null(expdata)) {
    exm <- pivot_timelines(expdata, k = k, with_ci = with_ci)
    rms <- oe_error(chr, expdata, time_limits)
  } else {
    rms <- NULL
  }
  title <- ifelse(is.null(title), "Model", title)
  param_str <- ifelse(withpars, parse_string_txt(chr$pars, rms), "")
  g <- timeline_panel(m, single = TRUE, ...)
  if(!is.null(expdata)) {
    g <- g +
      geom_step(data = exm, aes(colour = Colour), size = 0.2)
  }
  if(with_ci) {
    g <- g +
      geom_ribbon(data = exm, aes(ymin = conf_lo, ymax = conf_up, fill = Colour), alpha = 0.1)
  }
  g <- g + labs(title = paste0(title, "\n", param_str))

  g <- g + theme(plot.title = element_text(size = title_size))
  g
}




#' Convert cpars into vector
#'
#' Convert a cpars object into a vector of parameters used by \code{optim}.
#'
#' @param pars A \code{cpars} object.
#' @param parsel A character vector of parameter names selected for fitting.
#'
#' @return A selection of \code{pars} defined by \code{parsel} as named vector.
par_vector <- function(pars, parsel) {
  p <- as.numeric(pars[parsel])
  names(p) <- parsel
  return(p)
}

# convert a vector of parameters used by optim into cpars object



#' Convert vector into cpars
#'
#' Convert a vector of parameters used by optim into cpars object
#'
#' @param p A named vector of parameters.
#' @param pars Full \code{crpars} object.
#'
#' @return A \code{cpars} object with selected parameters updated from \code{p}.
vector_par <- function(p, pars) {
  pars[names(p)] <- p
  return(pars)
}


get_model_proportion <- function(chr) {
  count <- total <- colour <- Time <- model_prop <- NULL

  chr$cnt |>
    tidyr::pivot_longer(tidyselect::all_of(chr$colours), names_to = "colour", values_to = "count") |>
    dplyr::mutate(model_prop = count / total) |>
    dplyr::mutate(colour = factor(colour, levels = chr$colours)) |>
    dplyr::select(c(Time, colour, model_prop))
}

# Find errors of proportion for experimental data
get_proportion_errors <- function(echr, conf.limit = 0.6827) {
  count <- total <- data <- ci <- res <- Time <- colour <- prop <- lo <- up <- NULL

  echr$cnt |>
    tidyr::pivot_longer(tidyselect::all_of(echr$colours), names_to = "colour", values_to = "count") |>
    tidyr::nest(data = c(count, total)) |>
    dplyr::mutate(ci = purrr::map(data, ~PropCIs::exactci(.$count, .$total, conf.limit))) |>
    dplyr::mutate(res = purrr::map(ci, \(x) {tibble::tibble(lo = x[[1]][1], up = x[[1]][2])})) |>
    tidyr::unnest(c(data, res)) |>
    dplyr::mutate(prop = count / total) |>
    dplyr::select(Time, colour, count, total, prop, lo, up) |>
    dplyr::mutate(colour = factor(colour, levels = echr$colours))
}



smooth_colours <- function(cnt, colours, k) {
  cnt[,colours] <- apply(cnt[,colours], 2, function(x) caTools::runmean(x, k))
  return(cnt)
}

squeeze <- function(p, s) {
  if(!is.null(s) && s > 0) p <- 0.5 + (p - 0.5) * (1 - 2 * s)
  p
}


smoothing_window <- function(cnt, k) {
  Time <- d_lo <- d_up <- delta <- NULL

  n <- nrow(cnt)
  k2 = k %/% 2
  cnt |>
    dplyr::select(Time) |>
    dplyr::mutate(
      d_lo = 0:(n - 1),
      d_up = (n - 1):0
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      delta = min(d_lo, d_up, k2),
      lo = Time - delta,
      up = Time + delta
    ) |>
    dplyr::select(-c(d_lo, d_up))
}


#' Melt timelines for plotting
#'
#' @param chr A \code{ChrCom3} object with data
#' @param label1 Name of the first variable in the grid
#' @param label2 Name of the second variable in the grid
#' @param k Smoothing half-window size. If zero, no smoothing.
#' @param with_ci Logical. If true, 95% confidence intervals of proportion are
#'   calculates (slow!).
#'
#' @return Melted data frame with proportions
pivot_timelines <- function(chr, label1 = "L1", label2 = "L2", k = 0, with_ci = TRUE) {
  # Binding variables from non-standard evaluation locally
  Time <- Colour <- prop <- total <- Count <- data <- conf.low <- conf.high <- NULL

  cnt <- chr$cnt
  colours <- as.character(chr$colours)

  # quick plot
  if(!with_ci) {
    cnt[ ,colours] <- cnt[ ,colours] / cnt$total
    if(k > 0) cnt <- smooth_colours(cnt, colours, k)

    res <- cnt |>
      tidyr::pivot_longer(-c(Time, total), names_to = "Colour", values_to = "prop") |>
      dplyr::mutate(Count = prop * total)
  } else {
    # Create window sizes for smoothing
    wnd <- smoothing_window(cnt, k)
    piv <- cnt |>
      tidyr::pivot_longer(-c(Time, total), names_to = "Colour", values_to = "Count")
    res <- purrr::map_dfr(1:nrow(wnd), function(i) {
      w <- wnd[i, ]
      ps <- piv |>
        dplyr::filter(Time >= w$lo & Time <= w$up) |>
        dplyr::group_by(Colour) |>
        dplyr::summarise(Count = sum(Count)) |>
        dplyr::mutate(total = sum(Count))
      if(with_ci) {
        ps <- ps |>
          tidyr::nest(data = c(total, Count)) |>
          dplyr::mutate(res = purrr::map(data, ~PropCIs::exactci(.x$Count, .x$total, conf.level = 0.95) |> broom::glance())) |>
          tidyr::unnest(c(data, res)) |>
          dplyr::select(Colour, Count, total, conf_lo = conf.low, conf_up = conf.high)
      }
      ps |>
        tibble::add_column(Time = w$Time)
    })
  }

  res |> dplyr::mutate(prop = Count / total) |>
    dplyr::mutate(Colour = factor(Colour, levels = chr$colours)) |>
    tidyr::drop_na() |>
    dplyr::mutate(X = label1, Y = label2)
}



parse_string_txt <- function(pars, rms) {
  nms <- grep("squeeze|k3|tau3", names(pars), value=TRUE, invert=TRUE, perl=TRUE)
  txt <- paste0(lapply(nms, function(name) {
    paste0(name, " = ", sprintf("%.3g", as.numeric(pars[[name]])))
  }
  ), collapse=",  ")
  if(!is.null(rms)) txt <- paste0(txt, sprintf("\nrms=%.3g", rms))
  txt
}

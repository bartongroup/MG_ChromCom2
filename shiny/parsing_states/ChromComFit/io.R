#' Convert chromcom2 parsed data into \code{chromcom} object
#'
#' @details This function is needed to convert data between two different
#'   projects. The first one, codenanme \code{ChromCom} was about modelling
#'   transitions between three states (marked as blue, pink and red). The second
#'   one, codename \code{ChromCom2}, is about identifying states from 3D data.
#'   The number of states have been increased to none, black, lightblue, blue,
#'   brown, pink and red. In the conversion, none and black are rejected,
#'   lightblue, blue and brown are coerced into blue. Here, the parsed data
#'   table from \code{ChromCom2} is converted into \code{chromcom} object with
#'   appropriate colour shrinking.
#'
#' @param pd A tibble with parsed data
#'
#' @return A \code{chromcom} object with data extracted from \code{pd}.
#' @export
parsed_to_chrcom3 <- function(pd) {
  # Binding variables from non-standard evaluation locally
  letter <- time_nebd <- cell_id <- state <- new_letter <- NULL

  pars <- cpars()

  # Reduce states, browm, lightblue and blue -> blue, none, black -> remove
  pd <- pd |>
    dplyr::filter(!(state %in% c("none", "black"))) |>
    dplyr::mutate(new_letter = dplyr::recode(letter, W = "B", L = "B") |> as.character())
  tdat <- pd |>
    dplyr::arrange(time_nebd) |>
    tidyr::pivot_wider(id_cols = cell_id, names_from = time_nebd, values_from = new_letter) |>
    dplyr::select(-cell_id) |>
    as.matrix()
  tim <- colnames(tdat) |>
    as.numeric()

  chromcom(pars, time = tim, cells = tdat, colours = unique(pd$new_letter))
}

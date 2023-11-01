#' Colour-blind friendly palette of 7 colours
#' @export
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#' Colours for states
#' @export
STATE_COLOUR <- tibble::tribble(
  ~state,      ~colour,      ~letter,
  "darkblue",  "mediumblue", "B",
  "pink",      "lightpink",  "P",
  "red",       "red",        "R"
)
STATE_COLOUR$state <- factor(STATE_COLOUR$state, levels = STATE_COLOUR$state)



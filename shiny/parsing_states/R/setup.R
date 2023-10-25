library(tidyverse)

RED_PINK_RULES <- c(
  "(a < D & b < D) | (ang < A & (a < D | b < D))",
  "(a < D & b < D) & ang < A",
  "(a < D | b < D) & ang < A"
)

DEFAULT_PARAMS <- list(
  dist.black_lightblue = 0.4,
  dist.darkblue_brown = 0.75,
  dist.red_pink = 0.5,
  dist.brown_redpink = 0.5,
  black.length = 5,
  angle.red_pink = 30,
  rule.red_pink = RED_PINK_RULES[1]
)

STATE_COLOUR <- tribble(
  ~state, ~colour, ~letter,
  "none", "grey70", "-",
  "black", "black", "K",
  "lightblue", "skyblue", "L",
  "darkblue", "mediumblue", "B",
  "brown", "brown", "W",
  "pink", "lightpink", "P",
  "red", "red", "R"
) %>% 
  mutate(state = as_factor(state))

CELL_SHEETS <- c(
  "Intensity Sum Ch=1 Img=1",
  "Intensity Sum Ch=2 Img=1",
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Max Ch=1 Img=1",
  "Intensity Max Ch=2 Img=1",
  "Volume",
  "Time",
  "Position"
)

EXTVOL_SHEETS <- c(
  "Intensity Sum Ch=1 Img=1",
  "Intensity Sum Ch=2 Img=1",
  "Intensity Min Ch=1 Img=1",
  "Intensity Min Ch=2 Img=1",
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Median Ch=1 Img=1",
  "Intensity Median Ch=2 Img=1",
  "Volume",
  "Time"
)

CONDITIONS <- tibble(
  condition = c("NCAPD2_siRNA", "NCAPD3_siRNA", "no_siRNA")
)

col_con <- tibble::tribble(
  ~old, ~new,
  "green", "green",
  "red", "red",
  "550", "green",
  "647", "red"
)

COLOUR_CONVERSION <- setNames(col_con$new, col_con$old)


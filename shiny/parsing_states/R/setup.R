library(tidyverse)

DEFAULT_PARAMS <- list(
  dist.black_lightblue = 0.4,
  dist.darkblue_brown = 0.75,
  dist.red_pink = 0.5,
  dist.brown_redpink = 0.5,
  black.length = 5,
  angle.red_pink = 30,
  rule.red_pink = "(a < D & b < D) | (ang < A & (a < D | b < D))"
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
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Max Ch=1 Img=1",
  "Intensity Max Ch=2 Img=1",
  "Time",
  "Position"
)

BACKGROUND_SHEETS <- c(
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Median Ch=1 Img=1",
  "Intensity Median Ch=2 Img=1",
  "Time"
)


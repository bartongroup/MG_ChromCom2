library(tidyverse)

state_colour <- tribble(
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

cell_sheets <- c(
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Max Ch=1 Img=1",
  "Intensity Max Ch=2 Img=1",
  "Time",
  "Position"
)

background_sheets <- c(
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Median Ch=1 Img=1",
  "Intensity Median Ch=2 Img=1",
  "Time"
)


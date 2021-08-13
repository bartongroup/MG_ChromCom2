library(targets)

packages <- c("glue", "cowplot", "ggrepel", "ggbeeswarm", "ggridges", "readxl", "gganimate", "tidyverse")
tar_option_set(packages = packages, format = "qs")
options(dplyr.summarise.inform = FALSE)


# for interactive session only
# sapply(packages, library, character.only=TRUE)

files_R <- list.files("R", pattern="*.R$", full.names=TRUE)
sr_ <- sapply(files_R, source)

files_shiny <- list.files("shiny/parsing_states/R", pattern="*.R$", full.names=TRUE)
ss_ <- sapply(files_shiny, source)

sesinfo <- list(
  tar_target(session_info, sessionInfo())
)


c(
  sesinfo,
  my_targets()
)


options(dplyr.summarise.inform = FALSE)
source("R/packages.R")

files_R <- list.files("R", pattern="*.R$", full.names=TRUE)
sr_ <- sapply(files_R, source)

files_drake <- list.files("drake", pattern="*.R$", full.names=TRUE)
sd_ <- sapply(files_drake, source)


sesinfo <- drake_plan(
  session_info = sessionInfo()
)

plan <- bind_rows(
  sesinfo,
  plan_data(),
  plan_figures()
)

cfg <- drake_config(plan)


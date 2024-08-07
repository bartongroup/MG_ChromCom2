library(shiny)
library(shinycssloaders)
library(readxl)
library(caTools)
library(glue)
library(plotly)
library(cowplot)
library(tidyverse)
#library(ChromComParse)
#library(ChromComFit)

files_R <- list.files(c("R", "ChromComParse", "ChroComFit"), pattern = "*.R$", full.names = TRUE)
sr_ <- sapply(files_R, source)


options(dplyr.summarise.inform = FALSE)

# ChromCom2 functions

source("R/setup.R")
source("R/shiny_func.R")

# App modules

source("modules/parse_data.R")
source("modules/main_plot.R")
source("modules/timeline.R")
source("modules/dots.R")


# Use local data if testing

dirs <- c(
  "/cluster/gjb_lab/mgierlinski/projects/chromcom2/data",
  "/Users/mgierlinski/Projects/ChromCom2/data",
  "/cluster/gjb_lab/mgierlinski/ShinyApps/private/chromcom2/data"
)
DATA_PATH <- NULL
for (d in dirs) if (dir.exists(d)) DATA_PATH <- d

# Cache file to store processed Excel sheets

cache_path <- "cache"
CACHE_FILE <- file.path(cache_path, "data.rds")

if (!dir.exists(cache_path)) dir.create(cache_path)
if (!file.exists(CACHE_FILE)) {
  reload_data(DATA_PATH, CELL_SHEETS, EXTVOL_SHEETS, CACHE_FILE)
}

# Initial parameters for selectInput

initial_dat <- read_rds(CACHE_FILE)
initial_cells <- initial_cellcons(initial_dat$metadata)



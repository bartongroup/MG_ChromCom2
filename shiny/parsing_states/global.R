# set libpath for our Shiny server

libDir <- "/cluster/gjb_lab/mgierlinski/R_shiny/library/4.1"
if(dir.exists(libDir)) .libPaths(libDir)

library(shiny)
library(shinycssloaders)
library(readxl)
library(caTools)
library(glue)
library(plotly)
library(cowplot)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)

# ChromCom2 functions

source("R/setup.R")
source("R/io.R")
source("R/parse.R")
source("R/process.R")
source("R/shiny_func.R")

# App modules

source("modules/parse_data.R")
source("modules/main_plot.R")
source("modules/timeline.R")
source("modules/dots.R")


# Use local data if testing

dirs <- c(
  "/cluster/gjb_lab/mgierlinski/projects/chromcom2/data",
  "/Users/mgierlinski/Projects/ChromCom2/data"
)
data_path <- NULL
for(d in dirs) if(dir.exists(d)) data_path <- d

# Cache file to store processed Excel sheets

cache_path <- "cache"
cache_file <- file.path(cache_path, "data.rds")

if(!dir.exists(cache_path)) dir.create(cache_path)
if(!file.exists(cache_file)) {
  reload_data(data_path, cell_sheets, cache_file)
}

# Initial parameters for selectInput

initial_dat <- read_rds(cache_file)
initial_pars <- initial_parameters(initial_dat$metadata)
min_time <- min(initial_dat$xyz$time_nebd)
max_time <- max(initial_dat$xyz$time_nebd)

red_pink_rules <- c(
  "(a < D & b < D) | (ang < A & (a < D | b < D))",
  "(a < D & b < D) & ang < A",
  "(a < D | b < D) & ang < A"
)

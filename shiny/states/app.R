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

source("../setup.R")
source("../io.R")
source("../parse.R")
source("../process.R")
source("func.R")

options(dplyr.summarise.inform = FALSE)

# Use local data if testing
dirs <- c(
  "/cluster/gjb_lab/mgierlinski/projects/chromcom2/data",
  "/Users/mgierlinski/Projects/ChromCom2/data"
)
data.path <- NULL
for(d in dirs) if(dir.exists(d)) data.path <- d

# Chache file to store processed Excel sheets
cache.path <- "cache"
cache.file <- file.path(cache.path, "data.rds")

if(!dir.exists(cache.path)) dir.create(cache.path)
if(!file.exists(cache.file)) {
  reload_data(data.path, cell_sheets, cache.file)
}

# Initial parameters for selectInput
initial_dat <- read_rds(cache.file)
initial_pars <- initial_parameters(initial_dat$metadata)
min_time <- min(initial_dat$xyz$time_nebd)
max_time <- max(initial_dat$xyz$time_nebd)

red_pink_rules <- c("a_and_b_and_angle", "a_or_b_and_angle")


#########################################

ui <- fluidPage(
  titlePanel("Parsing states from cell images"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("reload", "Reload data"),
      hr(),
      #sliderInput("dist.black_lightblue", "Black/light blue limit", value=0.5, min=0, max=5, step=0.05, ticks=FALSE),
      sliderInput("dist.darkblue_brown", "Blue/brown limit", value=0.75, min=0, max=5, step=0.05, ticks=FALSE),
      sliderInput("dist.red_pink", "Red/pink limit", value=0.5, min=0, max=5, step=0.05, ticks=FALSE),
      #sliderInput("black.length", "Black length", value=5, min=0, max=10, step=1, ticks=FALSE),
      sliderInput("dist.brown_redpink", "Brown/red-pink limit", value=0.5, min=0, max=1, step=0.05, ticks=FALSE),
      sliderInput("angle.red_pink", "Red/pink angle", value=30, min=0, max=90, step=1, ticks=FALSE),
      selectInput("rule.red_pink", "Red/pink rule", choices=red_pink_rules),
      #checkboxInput("merge.blue", "Merge light/dark blue", value=TRUE),
      actionButton("submit", "Submit"),
      hr(),
      selectInput("cellcon", "Cell line/condition", choices=initial_pars$cellcons)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
        tabPanel("Overview",
          sliderInput("windowsize", "Running mean window", min=1, max=30, value=20, step=1, round=TRUE, ticks=FALSE),
          plotOutput("main_plot", height="600px") %>%
            withSpinner(color="#0dc5c1", type=5, size=0.5)
        ),
        
        tabPanel("Timeline",
          selectInput("mcell", "Movie/cell no.", choices=initial_pars$mcells),
          plotOutput("state_timeline", height="500px") %>%
            withSpinner(color="#0dc5c1", type=5, size=0.5)
        ),
        
        tabPanel("Dots",
          #sliderInput("time", "Time since NEBD (min)", value=0, min=min_time, max=max_time, step=1, round=TRUE, width="100%", ticks=FALSE),
          plotOutput("cell_map", height="300px", click="cell_time_click"),
          plotlyOutput("dot_plot", height="400px", width="400px") %>%
            withSpinner(color="#0dc5c1", type=5, size=0.5)
        )
        
      )
    )
  )
)


server <- function(input, output, session) {
  
  # We use isolate() to avoid dependency on input
  # The plot will be updated only on "sumbit"
  params_from_input <- function() {
    isolate(list(
      #dist.black_lightblue = input$dist_lightblue,
      dist.black_lightblue = 0,
      dist.darkblue_brown = input$dist.darkblue_brown,
      dist.brown_redpink = input$dist.brown_redpink,
      dist.red_pink = input$dist.red_pink,
      #black.length = input$black.length,
      black.length = 0,
      angle.red_pink = input$angle.red_pink,
      #merge.blue = input$merge.blue,
      merge.blue = TRUE,
      rule.red_pink = input$rule.red_pink
    ))
  }
  
  observeEvent(input$reload, {
    withProgress(message = "Loading Excel files", {
      info <- get_info(data.path)
      incProgress(1/4)
      dat <- read_cells(info, cell_sheets)
      incProgress(1/4)
      dat <- process_raw_data(dat, with_celldat=FALSE)
      incProgress(1/4)
      write_rds(dat, cache.file)
      incProgress(1/4)
      pars <- initial_parameters(dat$metadata)
      updateSelectInput(session, "cellcon", choices=pars$cellcons)
    })
  })
  
  observeEvent(input$cellcon, {
    d <- dat()
    mcells <- d$metadata %>% 
      filter(cellcon == input$cellcon) %>% 
      pull(mcell)
    updateSelectInput(session, "mcell", choices=mcells)
  })
  
  dat <- eventReactive(input$submit, {
    if(file.exists(cache.file)) {
      params <- params_from_input()
      read_rds(cache.file) %>% 
        parse_xyz_data(params)
    }
  })
  
  output$state_timeline <- renderPlot({
    input$submit
    d <- dat()
    params <- params_from_input()
    dp <- d$parsed %>% filter(cellcon == input$cellcon & mcell == input$mcell)
    plot_grid(pl_state_distance_timeline(dp, params), pl_all_distance_timeline(dp, params), ncol=1, align="v")
  })
  
  output$main_plot <- renderPlot({
    input$submit
    d <- dat()
    params <- params_from_input()
    dp <- d$parsed %>% filter(cellcon == input$cellcon)
    plot_grid(pl_state_map(dp, params), pl_proportion_map(dp, k=input$windowsize, params), ncol=1, align="v")
  })
  
  output$cell_map <- renderPlot({
    input$submit
    d <- dat()
    params <- params_from_input()
    dp <- d$parsed %>% filter(cellcon == input$cellcon)
    pl_state_map(dp, params)
  })
  
  output$dot_plot <- renderPlotly({
    input$submit
    d <- dat()
    params <- params_from_input()
    mp <- d$parsed %>% filter(cellcon == input$cellcon) %>% make_state_map(params)
    pl <- NULL
    if(!is.null(input$cell_time_click)) {
      sel <- nearPoints(mp, input$cell_time_click, xvar="x", yvar="y", maxpoints=1, threshold=100)
      xyz <- d$xyz %>% 
        filter(cellcon == sel$cellcon & mcell == sel$mcell & time_nebd == sel$time_nebd)
      pl <- plot_dots(xyz)
    }
    return(pl)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)



# input = list(dist.black_lightblue = 0.4, dist.darkblue_brown = 0.75, dist.red_pink = 0.4, black.length = 5)
libDir <- "/cluster/gjb_lab/mgierlinski/R_shiny/library/3.6"
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


#########################################

ui <- fluidPage(
  titlePanel("Parsing states from cell images"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("reload", "Reload data"),
      hr(),
      sliderInput("dist.lightblue", "Black/light blue limit", value=0.5, min=0, max=5, step=0.05, ticks=FALSE),
      sliderInput("dist.brown", "Blue/brown limit", value=0.75, min=0, max=5, step=0.05, ticks=FALSE),
      sliderInput("dist.pink", "Red/pink limit", value=0.4, min=0, max=5, step=0.05, ticks=FALSE),
      sliderInput("black.length", "Black length", value=5, min=0, max=10, step=1, ticks=FALSE),
      actionButton("submit", "Submit"),
      hr(),
      selectInput("cellcon", "Cell line/condition", choices=initial_pars$cellcons)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
        tabPanel("Overview",
          sliderInput("windowsize", "Running mean window", min=1, max=30, value=20, step=1, round=TRUE, ticks=FALSE),
          plotOutput("map", height="600px") %>%
            withSpinner(color="#0dc5c1", type=5, size=0.5)
        ),
        
        tabPanel("Timeline",
          selectInput("mcell", "Movie/cell no.", choices=initial_pars$mcells),
          plotOutput("state_timeline", height="500px") %>%
            withSpinner(color="#0dc5c1", type=5, size=0.5)
        ),
        
        tabPanel("Dots",
          sliderInput("time", "Time since NEBD (min)", value=0, min=min_time, max=max_time, step=1, round=TRUE, width="100%", ticks=FALSE),
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
      dist.lightblue = input$dist.lightblue,
      dist.brown = input$dist.brown,
      dist.pink = input$dist.pink,
      black.length = input$black.length
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
    dp <- d$parsed %>% 
      filter(cellcon == input$cellcon & mcell == input$mcell)
    plot_grid(pl_state_distance_timeline(dp, params), pl_all_distance_timeline(dp, params), ncol=1, align="v")
  })
  
  output$map <- renderPlot({
    input$submit
    d <- dat()
    dp <- d$parsed %>% 
      filter(cellcon == input$cellcon)
    plot_grid(pl_state_map(dp), pl_proportion_map(dp, k=input$windowsize), ncol=1, align="v")
  })
  
  output$dot_plot <- renderPlotly({
    d <- dat()
    d$xyz %>% 
      filter(cellcon == input$cellcon & mcell == input$mcell & time_nebd == input$time) %>% 
      plot_ly() %>%
      add_trace(type="scatter3d", mode="markers", x = ~x, y = ~y, z = ~z, marker=list(color = ~colour)) %>% 
      layout(font=list(size=9), scene=list(aspectmode="data"))
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)



# input = list(dist.lightblue = 0.4, dist.brown = 0.75, dist.pink = 0.4, black.length = 5)
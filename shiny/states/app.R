library(shiny)
library(shinycssloaders)
library(tidyverse)

source("../setup.R")
source("../io.R")
source("../parse.R")
source("../process.R")
source("func.R")

data.path <- "../../data"
cache.path <- "cache"

if(!dir.exists(cache.path)) {
  dir.create(cache.path)
  reload_data(data.path, cell_sheets, cache.file)
}
initial_dat <- read_rds(cache.file)
initial_conditions <- initial_dat$metadata$condition %>% levels()
initial_cells <- initial_dat$metadata$cell %>% unique()


#########################################

ui <- fluidPage(
  titlePanel("Parsing states from cell images"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("reload", "Reload data"),
      hr(),
      sliderInput("dist.lightblue", "Black/light blue limit", value=0.5, min=0, max=5, step=0.05),
      sliderInput("dist.brown", "Blue/brown limit", value=0.75, min=0, max=5, step=0.05),
      sliderInput("dist.pink", "Red/pink limit", value=0.4, min=0, max=5, step=0.05),
      sliderInput("black.length", "Black length", value=5, min=0, max=10, step=1),
      actionButton("submit", "Submit"),
      hr(),
      selectInput("condition", "Condition", choices=initial_conditions),
      selectInput("cell", "Cell number", choices=initial_cells),
    ),
    
    mainPanel(
      plotOutput("heatmap", height="100px") %>% withSpinner(color="#0dc5c1"),
      plotOutput("dist_state_plot", height="300px") %>% withSpinner(color="#0dc5c1")
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
    })
  })
  
  dat <- eventReactive(input$submit, {
    if(file.exists(cache.file)) {
      params <- params_from_input()
      read_rds(cache.file) %>% 
        parse_xyz_data(params)
    }
  })
  
  output$dist_state_plot <- renderPlot({
    input$submit
    d <- dat()
    params <- params_from_input()
    d$parsed %>% 
      filter(condition == input$condition & cell == input$cell) %>% 
      pl_state_distance(params)
  })
  
  output$heatmap <- renderPlot({
    input$submit
    d <- dat()
    d$parsed %>% 
      filter(condition == input$condition) %>% 
      pl_state_map()
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)



# input = list(dist.lightblue = 0.4, dist.brown = 0.75, dist.pink = 0.4, black.length = 5, condition = "TT206", cell = 1)
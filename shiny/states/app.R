library(shiny)
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
      p(),
      selectInput("condition", "Conditions", choices=initial_conditions),
      selectInput("cell", "Cell number", choices=initial_cells),
      sliderInput("dist.lightblue", "Black/light blue limit", value=0.5, min=0, max=5, step=0.05),
      sliderInput("dist.brown", "Blue/brown limit", value=0.75, min=0, max=5, step=0.05),
      sliderInput("dist.pink", "Red/pink limit", value=0.4, min=0, max=5, step=0.05),
      sliderInput("black.length", "Black length", value=5, min=0, max=10, step=1),
      actionButton("submit", "Submit"),
    ),
    
    mainPanel(
      plotOutput("dist_state_plot", height="300px")
    )
  )
)


server <- function(input, output, session) {
  
  params_from_input <- function() {
    list(
      dist.lightblue = input$dist.lightblue,
      dist.brown = input$dist.brown,
      dist.pink = input$dist.pink,
      black.length = input$black.length
    )
  }
  
  observeEvent(input$reload, {
    reload_data(data.path, cell_sheets, cache.file)
  })
  
  dat <- eventReactive(input$submit, {
    if(file.exists(cache.file)) {
      params <- params_from_input()
      read_rds(cache.file) %>% 
        parse_xyz_data(params)
    }
  })
  
  output$dist_state_plot <- renderPlot({
    d <- dat()
    params <- params_from_input()
    d$parsed %>% 
      filter(condition == input$condition & cell == input$cell) %>% 
      pl_state_distance(params)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)



# input = list(dist.lightblue = 0.4, dist.brown = 0.75, dist.pink = 0.4, black.length = 5, condition = "TT206", cell = 1)
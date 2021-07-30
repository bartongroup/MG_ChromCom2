# Shiny module main_plot
# Creates the main cell-line plot

# ----- UI definitions -----

mod_main_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("windowsize"), "Running mean window", min=1, max=30, value=20, step=1, round=TRUE, ticks=FALSE),
    plotOutput(ns("main_plot"), height="600px") %>%
      withSpinner(color="#0dc5c1", type=5, size=0.5)
  )
}

# ----- Server logic -----

mod_main_plot <- function(id, state) {
  
  server <- function(input, output, session) {

    output$main_plot <- renderPlot({
      req(state$is_parsed)
      d <- state$data
      con <- state$cellcon
      dp <- d$parsed %>% filter(cellcon == con)   # putting state$cellcon here crashes the app (???)
      plot_grid(
        pl_state_map(dp, d$params),
        pl_proportion_map(dp, k=input$windowsize, d$params),
        ncol=1, align="v"
      )
    })
    
  }
  
  moduleServer(id, server)
}
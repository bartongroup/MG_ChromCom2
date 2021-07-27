mod_main_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("windowsize"), "Running mean window", min=1, max=30, value=20, step=1, round=TRUE, ticks=FALSE),
    plotOutput(ns("main_plot"), height="600px") %>%
      withSpinner(color="#0dc5c1", type=5, size=0.5)
  )
}


mod_main_plot <- function(id, dat, submit_button, cellcon) {
  
  server <- function(input, output, session) {
    ns <- session$ns

    output$main_plot <- renderPlot({
      # Take a dependency on submit button
      submit_button()
      d <- dat()
      dp <- d$parsed %>% filter(cellcon == cellcon())
      plot_grid(
        pl_state_map(dp, d$params),
        pl_proportion_map(dp, k=input$windowsize, d$params),
        ncol=1, align="v"
      )
    })
    
  }
  
  moduleServer(id, server)
}
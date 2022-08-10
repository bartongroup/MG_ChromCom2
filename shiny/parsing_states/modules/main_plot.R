# Shiny module main_plot
# Creates the main cell-line plot

# ----- UI definitions -----

mod_main_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sliderInput(ns("windowsize"), "Running mean window", min = 1, max = 30, value = 20, step = 1, round = TRUE, ticks = FALSE),
    plotOutput(ns("main_plot"), height = "600px") %>%
      withSpinner(color = "#0dc5c1", type = 5, size = 0.5),
    downloadButton(ns("download_data"), "Download")
  )
}

# ----- Server logic -----

mod_main_plot <- function(id, state) {
  
  server <- function(input, output, session) {
    
    get_dp <- reactive({
      req(state$is_parsed)
      con <- state$cellcon
      state$data$parsed %>% filter(cellcon == con)   # putting state$cellcon here crashes the app (???)
    })

    output$main_plot <- renderPlot({
      dp <- get_dp()
      plot_grid(
        pl_state_map(dp, state$data$params),
        pl_proportion_map(dp, k = input$windowsize, state$data$params),
        ncol = 1, align = "v"
      )
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        paste0(state$cellcon, ".csv")
      },
      content = function(file) {
        get_dp() %>% 
          make_proportion_map(k = input$windowsize, state$data$params) %>% 
          write_csv(file)
      }
    )
    
  }
  
  moduleServer(id, server)
}
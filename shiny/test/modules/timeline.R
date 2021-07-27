mod_timeline_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("mcell"), "Movie/cell no.", choices=initial_pars$mcells),
    plotOutput(ns("timeline"), height="500px") %>%
      withSpinner(color="#0dc5c1", type=5, size=0.5)
  )
}


mod_timeline <- function(id, dat, submit_button, cellcon) {
  
  server <- function(input, output, session) {
    ns <- session$ns
    
    observeEvent(cellcon(), {
      mcells <- dat()$metadata %>% 
        filter(cellcon == cellcon()) %>% 
        pull(mcell)
      updateSelectInput(session, "mcell", choices=mcells)
    })
    
    output$timeline <- renderPlot({
      # Take a dependency on input$submit
      submit_button()
      d <- dat()
      dp <- d$parsed %>% filter(cellcon == cellcon() & mcell == input$mcell)
      plot_grid(
        pl_state_distance_timeline(dp, d$params),
        pl_all_distance_timeline(dp, d$params),
        ncol=1, align="v"
      )
    })
    


  }
  
  moduleServer(id, server)
}
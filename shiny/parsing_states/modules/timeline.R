# Shiny module timeline
# Create a timeline for a selected cell

# ----- UI definitions -----

mod_timeline_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("mcell"), "Day/movie/cell no.", choices=initial_cells$mcells),
    plotOutput(ns("timeline"), height="500px") %>%
      withSpinner(color="#0dc5c1", type=5, size=0.5)
  )
}

# ----- Server logic -----

mod_timeline <- function(id, state) {
  
  server <- function(input, output, session) {

    # If cell/condition is changed, update movie/cell number selector
    observeEvent(state$cellcon, {
      req(state$is_parsed)
      mcells <- state$data$metadata %>% 
        filter(cellcon == state$cellcon) %>% 
        pull(mcell)
      updateSelectInput(session, "mcell", choices=mcells)
    })
    
    output$timeline <- renderPlot({
      req(state$is_parsed)
      d <- state$data
      con <- state$cellcon
      dp <- d$parsed %>% filter(cellcon == con & mcell == input$mcell)
      # delays in selectInput updates might result in wrong mcell selection for
      # a short time, this is to avoid an error message on screen
      req(nrow(dp) > 0)  
      plot_grid(
        pl_state_distance_timeline(dp, d$params),
        pl_all_distance_timeline(dp, d$params),
        ncol=1, align="v"
      )
    })
    


  }
  
  moduleServer(id, server)
}
# Server for Shiny app "Parsing states"

server <- function(input, output, session) {
  
  # Initialise app state
  app_state <- reactiveValues(
    data = NULL,
    is_parsed = FALSE,
    cellcon = initial_cells$cellcons[1]
  )
  
  # Need to pass cell-line/condition down to modules
  observeEvent(input$cellcon, app_state$cellcon <- input$cellcon)

  # server logic
  mod_parse_data("parse_data", app_state)
  mod_main_plot("main_plot", app_state)
  mod_timeline("state_timeline", app_state)
  mod_dots("dots", app_state)
  
}


server <- function(input, output, session) {
  
  parsed_data <- mod_parse_data("parse_data")

  mod_main_plot("main_plot", parsed_data, reactive(input$submit), reactive(input$cellcon))
  mod_timeline("state_timeline", parsed_data, reactive(input$submit), reactive(input$cellcon))
  mod_dots("dots", parsed_data, reactive(input$submit), reactive(input$cellcon))
  
}

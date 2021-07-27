
server <- function(input, output, session) {
  
  dat <- mod_parse_data("parse_data")

  mod_main_plot("main_plot", dat, reactive(input$submit), reactive(input$cellcon))
  mod_timeline("state_timeline", dat, reactive(input$submit), reactive(input$cellcon))
  mod_dots("dots", dat, reactive(input$submit), reactive(input$cellcon))
  
}

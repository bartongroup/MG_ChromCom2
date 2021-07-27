ui <- fluidPage(
  titlePanel("Parsing states from cell images"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      mod_parse_data_ui("parse_data"),
      hr(),
      selectInput("cellcon", "Cell line/condition", choices=initial_pars$cellcons),
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Overview", mod_main_plot_ui("main_plot")),
        tabPanel("Timeline", mod_timeline_ui("state_timeline")),
        tabPanel("Dots", mod_dots_ui("dots"))
      )
    )
  )
)

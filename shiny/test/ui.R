ui <- fluidPage(
  titlePanel("Parsing states from cell images"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      mod_params_data_ui("parse_data")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
        tabPanel("Overview",
          sliderInput("windowsize", "Running mean window", min=1, max=30, value=20, step=1, round=TRUE, ticks=FALSE),
          plotOutput("main_plot", height="600px") %>%
            withSpinner(color="#0dc5c1", type=5, size=0.5)
        ),
                  
        tabPanel("Timeline",
          selectInput("mcell", "Movie/cell no.", choices=initial_pars$mcells),
          plotOutput("state_timeline", height="500px") %>%
            withSpinner(color="#0dc5c1", type=5, size=0.5)
        ),
                  
        tabPanel("Dots",
          plotOutput("cell_map", height="300px", click="cell_time_click"),
          plotlyOutput("dot_plot", height="400px", width="400px"),
          tableOutput("dot_info")
        )
                  
      )
    )
  )
)

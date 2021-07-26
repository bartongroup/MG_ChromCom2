ui <- fluidPage(
  titlePanel("Parsing states from cell images"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("reload", "Reload data"),
      hr(),
      #sliderInput("dist.black_lightblue", "Black/light blue limit", value=0.5, min=0, max=5, step=0.05, ticks=FALSE),
      #sliderInput("black.length", "Black length", value=5, min=0, max=10, step=1, ticks=FALSE),
      sliderInput("dist.darkblue_brown", "Blue/brown limit", value=0.75, min=0, max=5, step=0.05, ticks=FALSE),
      sliderInput("dist.brown_redpink", "Brown/red-pink limit", value=0.5, min=0, max=1, step=0.05, ticks=FALSE),
      sliderInput("dist.red_pink", "Red/pink limit (D)", value=0.5, min=0, max=5, step=0.05, ticks=FALSE),
      sliderInput("angle.red_pink", "Red/pink angle (A)", value=30, min=0, max=90, step=1, ticks=FALSE),
      selectInput("rule.red_pink", "Red/pink rule", choices=red_pink_rules),
      #checkboxInput("merge.blue", "Merge light/dark blue", value=TRUE),
      actionButton("submit", "Submit"),
      hr(),
      selectInput("cellcon", "Cell line/condition", choices=initial_pars$cellcons)
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

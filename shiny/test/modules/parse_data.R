# Shiny module mod_parse_data
# Read input parameters and parse distance data to create states

# ----- UI definitions -----

mod_parse_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("reload"), "Reload data"),
    hr(),
    sliderInput(ns("dist.darkblue_brown"), "Blue/brown limit", value=0.75, min=0, max=5, step=0.05, ticks=FALSE),
    sliderInput(ns("dist.brown_redpink"), "Brown/red-pink limit", value=0.5, min=0, max=1, step=0.05, ticks=FALSE),
    sliderInput(ns("dist.red_pink"), "Red/pink limit (D)", value=0.5, min=0, max=5, step=0.05, ticks=FALSE),
    sliderInput(ns("angle.red_pink"), "Red/pink angle (A)", value=30, min=0, max=90, step=1, ticks=FALSE),
    selectInput(ns("rule.red_pink"), "Red/pink rule", choices=red_pink_rules),
    actionButton(ns("submit"), "Submit")
  )
}

# ----- Server logic -----

mod_parse_data <- function(id, state) {
  server <- function(input, output, session) {

    # We use isolate() to avoid dependency on input
    # The plot will be updated only on "sumbit"
    params_from_input <- function() {
      isolate(list(
        #dist.black_lightblue = input$dist_lightblue,
        dist.black_lightblue = 0,
        dist.darkblue_brown = input$dist.darkblue_brown,
        dist.brown_redpink = input$dist.brown_redpink,
        dist.red_pink = input$dist.red_pink,
        #black.length = input$black.length,
        black.length = 0,
        angle.red_pink = input$angle.red_pink,
        #merge.blue = input$merge.blue,
        merge.blue = TRUE,
        rule.red_pink = input$rule.red_pink
      ))
    }
    
    # React to "reload" button by loading and processing all Excel files
    observeEvent(input$reload, {
      withProgress(message = "Loading Excel files", {
        info <- get_info(data_path)
        incProgress(1/4)
        dat <- read_cells(info, cell_sheets)
        incProgress(1/4)
        dat <- process_raw_data(dat, with_celldat=FALSE)
        incProgress(1/4)
        write_rds(dat, cache_file)
        incProgress(1/4)
        pars <- initial_parameters(dat$metadata)
        updateSelectInput(session, "cellcon", choices=pars$cellcons)
      })
    })
    
    # On "submit" parse and return parsed data
    eventReactive(input$submit, {
      req(file.exists(cache_file))
      params <- params_from_input()
      read_rds(cache_file) %>% 
        parse_xyz_data(params)
    })
  }
  
  
  moduleServer(id, server)
  
}
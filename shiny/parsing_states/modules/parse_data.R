# Shiny module mod_parse_data
# Read input parameters and parse distance data to create states

# ----- UI definitions -----

mod_parse_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    # For initially disabled download button
    tags$head(
      tags$style(HTML("
        a.shiny-download-link.disabled,
        a.shiny-download-link[disabled] {
          pointer-events: none;
          opacity: 0.65;
        }
      "))
    ),
    splitLayout(
      sliderInput(
        inputId = ns("dist.black_lightblue"),
        label = "Black/blue limit",
        value = DEFAULT_PARAMS$dist.black_lightblue,
        min = 0,
        max = 5,
        step = 0.05,
        ticks = FALSE
      ),
      sliderInput(
        inputId = ns("black.length"),
        label = "Black length",
        value = DEFAULT_PARAMS$black.length,
        min = 0,
        max = 20,
        step = 1,
        ticks = FALSE
      )),
    splitLayout(
      sliderInput(
        inputId = ns("dist.darkblue_brown"),
        label = "Blue/brown limit",
        value = DEFAULT_PARAMS$dist.darkblue_brown,
        min = 0,
        max = 5,
        step = 0.05,
        ticks = FALSE
      ),
      sliderInput(
        inputId = ns("dist.brown_redpink"),
        label = "Brown/red-pink limit",
        value = DEFAULT_PARAMS$dist.brown_redpink,
        min = 0,
        max = 1,
        step = 0.05,
        ticks = FALSE
      )),
    splitLayout(
      sliderInput(
        inputId = ns("dist.red_pink"),
        label = "Red/pink limit (D)",
        value = DEFAULT_PARAMS$dist.red_pink,
        min = 0,
        max = 5,
        step = 0.05,
        ticks = FALSE
      ),
      sliderInput(
        inputId = ns("angle.red_pink"),
        label = "Red/pink angle (A)",
        value = DEFAULT_PARAMS$angle.red_pink,
        min = 0,
        max = 90,
        step = 1,
        ticks = FALSE
      )),
    selectInput(
      inputId = ns("rule.red_pink"),
      label = "Red/pink rule",
      choices = RED_PINK_RULES
    ),
    actionButton(
      inputId = ns("submit"),
      label = "Submit"
    ),
    shinyjs::disabled(
      downloadButton(
        outputId = ns("download_data"),
        label = "Download data"
      )
    ),
    hr(),
    actionButton(
      inputId = ns("reload"),
      label = "Reload Excel files",
      style = "color: #fff; background-color: #cc6600; border-color: #cc3300"
    )
  )
}

# ----- Server logic -----

mod_parse_data <- function(id, state) {
  server <- function(input, output, session) {
    
    # We use isolate() to avoid dependency on input
    # The plot will be updated only on "sumbit"
    params_from_input <- function() {
      isolate(list(
        dist.black_lightblue = input$dist.black_lightblue,
        dist.darkblue_brown = input$dist.darkblue_brown,
        dist.brown_redpink = input$dist.brown_redpink,
        dist.red_pink = input$dist.red_pink,
        black.length = input$black.length,
        angle.red_pink = input$angle.red_pink,
        #merge.blue = input$merge.blue,
        merge.blue = TRUE,
        rule.red_pink = input$rule.red_pink
      ))
    }
    
    # React to "reload" button by loading and processing all Excel files
    observeEvent(input$reload, {
      withProgress(message = "Loading Excel files", {
        info <- get_info(DATA_PATH, COLOUR_CONVERSION)
        incProgress(1/4)
        dat <- read_cells(info, CELL_SHEETS, EXTVOL_SHEETS)
        incProgress(1/4)
        dat <- process_raw_data(dat)
        incProgress(1/4)
        write_rds(dat, CACHE_FILE)
        incProgress(1/4)
        updateSelectInput(session, "cellcon", choices = levels(dat$metadata$cellcon))
      })
    })
    
    # On "submit" parse and return parsed data
    observeEvent(input$submit, {
      req(file.exists(CACHE_FILE))
      params <- params_from_input()
      dat <- read_rds(CACHE_FILE) %>% 
        parse_xyz_data(params)
      state$data <- dat
      state$is_parsed <- TRUE
      shinyjs::enable("download_data")
    })

    output$download_data <- downloadHandler(
      filename = "distances.csv",
      content = function(file) {
        req(state$is_parsed)
        write_csv(state$data$parsed, file)
      }
    )

  }
  
  
  moduleServer(id, server)
  
}

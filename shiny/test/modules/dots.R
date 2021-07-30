# Shiny module to display dots from a single image

# ----- UI definitions -----

mod_dots_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("cell_map"), height="300px", click=ns("cell_time_click")),
    plotlyOutput(ns("dot_plot"), height="400px", width="400px"),
    tableOutput(ns("dot_info"))
  )
}

# ----- Server logic -----

mod_dots <- function(id, dat, submit_button, cellcon) {
  
  server <- function(input, output, session) {

    output$cell_map <- renderPlot({
      # Take a dependency on input$submit
      submit_button()
      
      d <- dat()
      dp <- d$parsed %>% filter(cellcon == cellcon())
      pl_state_map(dp, d$params)
    })
    
    get_map_click <- eventReactive(input$cell_time_click, {
      d <- dat()
      mp <- d$parsed %>% filter(cellcon == cellcon()) %>% make_state_map(d$params)
      nearPoints(mp, input$cell_time_click, xvar="x", yvar="y", maxpoints=1, threshold=5)
    })
    
    output$dot_plot <- renderPlotly({
      sel <- get_map_click()
      req(nrow(sel) > 0)
      
      d <- dat()
      d$xyz %>%
        filter(cellcon == sel$cellcon & mcell == sel$mcell & time_nebd == sel$time_nebd) %>% 
        plot_dots()
    })
    
    output$dot_info <- renderTable({
      sel <- get_map_click()
      req(nrow(sel) > 0)

      sel %>% 
        select(cell_id = mcell, time_nebd, n_dot, a = dist_a, b = dist_b, r = dist_r, g = dist_g, angle_ab, angle_rg, state) %>% 
        mutate(angle_ab = 180 * angle_ab / pi, angle_rg = 180 * angle_rg / pi)
    })

  }
  
  moduleServer(id, server)
}
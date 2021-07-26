
server <- function(input, output, session) {
  
  dat <- mod_parse_data("parse_data")
  
  observeEvent(input$cellcon, {
    d <- dat()
    mcells <- d$metadata %>% 
      filter(cellcon == input$cellcon) %>% 
      pull(mcell)
    updateSelectInput(session, "mcell", choices=mcells)
  })
  
  output$state_timeline <- renderPlot({
    # Take a dependency on input$submit
    input$submit
    
    d <- dat()
    dp <- d$parsed %>% filter(cellcon == input$cellcon & mcell == input$mcell)
    plot_grid(pl_state_distance_timeline(dp, d$params), pl_all_distance_timeline(dp, d$params), ncol=1, align="v")
  })
  
  output$main_plot <- renderPlot({
    # Take a dependency on input$submit
    input$submit
    
    d <- dat()
    dp <- d$parsed %>% filter(cellcon == input$cellcon)
    plot_grid(pl_state_map(dp, d$params), pl_proportion_map(dp, k=input$windowsize, d$params), ncol=1, align="v")
  })
  
  output$cell_map <- renderPlot({
    # Take a dependency on input$submit
    input$submit
    
    d <- dat()
    dp <- d$parsed %>% filter(cellcon == input$cellcon)
    pl_state_map(dp, d$params)
  })
  
  get_map_click <- eventReactive(input$cell_time_click, {
    req(input$cell_time_click)

    d <- dat()
    mp <- d$parsed %>% filter(cellcon == input$cellcon) %>% make_state_map(d$params)
    nearPoints(mp, input$cell_time_click, xvar="x", yvar="y", maxpoints=1, threshold=5)
  })
  
  output$dot_plot <- renderPlotly({
    sel <- get_map_click()
    if(nrow(sel) == 0) return(NULL)

    d <- dat()
    d$xyz %>%
      filter(cellcon == sel$cellcon & mcell == sel$mcell & time_nebd == sel$time_nebd) %>% 
      plot_dots()
  })
  
  
  output$dot_info <- renderTable({
    sel <- get_map_click()
    if(nrow(sel) == 0) return(NULL)
    
    sel %>% 
      select(cell_id = mcell, time_nebd, n_dot, a = dist_a, b = dist_b, r = dist_r, g = dist_g, angle_ab, angle_rg, state) %>% 
      mutate(angle_ab = 180 * angle_ab / pi, angle_rg = 180 * angle_rg / pi)
  })
}




# input = list(dist.black_lightblue = 0.4, dist.darkblue_brown = 0.75, dist.red_pink = 0.4, black.length = 5)
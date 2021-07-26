
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
  
  observeEvent(input$reload, {
    withProgress(message = "Loading Excel files", {
      info <- get_info(data.path)
      incProgress(1/4)
      dat <- read_cells(info, cell_sheets)
      incProgress(1/4)
      dat <- process_raw_data(dat, with_celldat=FALSE)
      incProgress(1/4)
      write_rds(dat, cache.file)
      incProgress(1/4)
      pars <- initial_parameters(dat$metadata)
      updateSelectInput(session, "cellcon", choices=pars$cellcons)
    })
  })
  
  dat <- eventReactive(input$submit, {
    if(file.exists(cache_file)) {
      params <- params_from_input()
      read_rds(cache_file) %>% 
        parse_xyz_data(params)
    }
  })
  
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
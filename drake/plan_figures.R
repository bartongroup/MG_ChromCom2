plan_figures <- function() {
  
  anims <- drake_plan(
    anim_cell_1 = animate_cell(dat$xyz, "cell_1")
  )
  
  cells <- drake_plan(
    fig_colour_ident_cell_1 = plot_colour_identification(dat$raw$cell_1),
    fig_colour_timeline_cell_1 = plot_colour_timeline(dat$raw$cell_1),
    fig_cells_state_distance = plot_state_distance(dat$parsed),
    fig_cells_distance_distribution = plot_distance_distribution(dat$parsed, cex=1.5),
    fig_cells_map = plot_state_map(dat$parsed)
  )
  
  bind_rows(
    anims,
    cells
  )
}
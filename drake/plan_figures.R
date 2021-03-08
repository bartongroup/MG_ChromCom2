plan_figures <- function() {
  
  anims <- drake_plan(
    anim_cell_1 = animate_cell(dat$xyz, "cell_1")
  )
  
  cells <- drake_plan(
    fig_colour_ident_cell_1 = plot_colour_identification(dat$celldat$`TT206_1NM-PP1_1_1_R3D_D3D_cell 1_fr1-68`),
    fig_colour_timeline_cell_1 = plot_colour_timeline(dat, "TT206", 1),
    fig_cells_state_distance = plot_state_distance(dat$parsed),
    fig_cells_distance_distribution = plot_distance_distribution(dat$parsed, cex=1.5),
    fig_cells_map = plot_state_map(dat$parsed)
  )
  
  raw_examples <- drake_plan(
    raw_ex_cell1_m30 = get_timepoint_raw_data(raw, dat$xyz, "TT206", 1, -30),
    raw_ex_cell1_m25 = get_timepoint_raw_data(raw, dat$xyz, "TT206", 1, -25),
    raw_ex_cell1_p5 = get_timepoint_raw_data(raw, dat$xyz, "TT206", 1, 5)
  )
  
  bind_rows(
    anims,
    cells,
    raw_examples
  )
}
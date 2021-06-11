plan_figures <- function() {
  
  anims <- drake_plan(
    anim_cell_1 = animate_cell(dat$xyz, "cell_1")
  )
  
  cells <- drake_plan(
    fig_colour_ident_cell_1 = plot_colour_identification(dat$celldat$`TT206-no_siRNA:1_1-1`),
    fig_colour_timeline_cell_1 = plot_colour_timeline(dat, "TT206-no_siRNA:1_1-1"),
    fig_cells_state_distance = plot_state_distance(dat$parsed %>% filter(condition=="no_siRNA"), dat$params),
    fig_all_distances = plot_all_distances(dat$parsed %>% filter(condition=="no_siRNA"), dat$params),
    fig_cells_distance_distribution = plot_distance_distribution(dat$parsed %>% filter(condition=="no_siRNA"), dat$params, cex=1.5),
    fig_cells_map = plot_state_map(dat$parsed %>% filter(condition=="no_siRNA")),
    fig_angle_distribution = plot_angle_distribution(dat$parsed),
    fig_angle_timeline = plot_angle_timeline(dat$parsed),
    fig_distance_angle = plot_distance_angle(dat$parsed, dat$params)
  )
  
  raw_examples <- drake_plan(
    raw_ex_cell1_m31 = get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", -31),
    raw_ex_cell1_m13 = get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", -13),
    raw_ex_cell1_p6 = get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", 6)
  )
  
  bind_rows(
    anims,
    cells,
    raw_examples
  )
}
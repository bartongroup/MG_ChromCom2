my_targets <- function() {
  
  setup <- list(
    tar_target(params, list(
      dist.black_lightblue = 0.4,
      dist.darkblue_brown = 0.75,
      dist.red_pink = 0.5,
      dist.brown_redpink = 0.5,
      black.length = 5,
      angle.red_pink = 30,
      rule.red_pink = "a_and_b_and_angle"
    ))
  )
  
  read_data <- list(
    tar_target(info, get_info("data")),
    tar_target(raw, read_cells(info, cell_sheets)),
    tar_target(dat, process_raw_data(raw) %>% parse_xyz_data(params))
  )
  
  figures_cells <- list(
    tar_target(fig_colour_ident_cell_1, plot_colour_identification(dat$celldat$`TT206-no_siRNA:1_1-1`)),
    tar_target(fig_colour_timeline_cell_1, plot_colour_timeline(dat, "TT206-no_siRNA:1_1-1")),
    tar_target(fig_cells_state_distance, plot_state_distance(dat$parsed %>% filter(condition=="no_siRNA"), dat$params)),
    tar_target(fig_all_distances, plot_all_distances(dat$parsed %>% filter(condition=="no_siRNA"), dat$params)),
    tar_target(fig_cells_distance_distribution, plot_distance_distribution(dat$parsed %>% filter(condition=="no_siRNA"), dat$params, cex=1.5)),
    tar_target(fig_cells_map, plot_state_map(dat$parsed %>% filter(condition=="no_siRNA"))),
    tar_target(fig_angle_distribution, plot_angle_distribution(dat$parsed)),
    tar_target(fig_angle_timeline, plot_angle_timeline(dat$parsed)),
    tar_target(fig_distance_angle, plot_distance_angle(dat$parsed, dat$params)),
    tar_target(fig_distance_angle_rg, plot_distance_angle(dat$parsed, dat$params, colour="rg")),
    tar_target(fig_distance_angle_timeline, plot_distance_angle(dat$parsed, dat$params, facet="win")),
    tar_target(fig_rg_angle, plot_rg_angle(dat$parsed, dat$params)),
    tar_target(fig_rg_angle_timeline, plot_rg_angle(dat$parsed, dat$params, facet="win"))
  )

  raw_examples <- list(
    tar_target(raw_ex_cell1_m31, get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", -31)),
    tar_target(raw_ex_cell1_m13, get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", -13)),
    tar_target(raw_ex_cell1_p6, get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", 6))
  )
  
  c(
    setup,
    read_data,
    raw_examples,
    figures_cells
  ) 
}
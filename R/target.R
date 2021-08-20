my_targets <- function() {
  
  read_data <- list(
    tar_target(info, get_info("data")),
    tar_target(raw, read_cells(info, CELL_SHEETS, BACKGROUND_SHEETS)),
    tar_target(dat, process_raw_data(raw) %>% parse_xyz_data(DEFAULT_PARAMS)),
    
    tar_target(info_pilot, get_info("background_pilot")),
    tar_target(raw_pilot, read_cells(info_pilot, CELL_SHEETS, BACKGROUND_SHEETS)),
    tar_target(dat_pilot, process_raw_data(raw_pilot) %>% parse_xyz_data(DEFAULT_PARAMS))
  )
  
  figures <- list(
    tar_target(fig_colour_ident_cell_1, plot_colour_identification(dat$dotsdat$`TT206-no_siRNA:1_1-1`)),
    tar_target(fig_colour_timeline_cell_1, plot_colour_timeline(dat, "TT206-no_siRNA:1_1-1")),
    tar_target(fig_angle_distribution, plot_angle_distribution(dat$parsed)),
    tar_target(fig_angle_timeline, plot_angle_timeline(dat$parsed)),
    tar_target(fig_distance_angle, plot_distance_angle(dat$parsed, dat$params)),
    tar_target(fig_distance_angle_rg, plot_distance_angle(dat$parsed, dat$params, colour="rg")),
    tar_target(fig_distance_angle_timeline, plot_distance_angle(dat$parsed, dat$params, facet="win")),
    tar_target(fig_rg_angle, plot_rg_angle(dat$parsed, dat$params)),
    tar_target(fig_rg_angle_timeline, plot_rg_angle(dat$parsed, dat$params, facet="win")),
    tar_target(fig_intensity_sn_combined, plot_intensity_sn_combined(dat_pilot)),
    tar_target(fig_intensity_mean_volume, plot_intensity_mean_volume(dat_pilot$intensities)),
    tar_target(fig_intensity_mean_sum, plot_intensity_sum(dat_pilot$intensities,  "TT206-NCAPD2_siRNA:2_2-3")),
    tar_target(fig_mean_sum_n, plot_mean_sum_n(dat_pilot$intensities))
  )
  
  figures_per_condition <- tar_map(values=CONDITIONS,
    tar_target(fig_cells_state_distance, plot_state_distance(dat$parsed %>% filter(condition==condition), dat$params)),
    tar_target(fig_all_distances, plot_all_distances(dat$parsed %>% filter(condition==condition), dat$params)),
    tar_target(fig_cells_distance_distribution, plot_distance_distribution(dat$parsed %>% filter(condition==condition), dat$params, cex=1.5)),
    tar_target(fig_cells_map, plot_state_map(dat$parsed %>% filter(condition==condition))),
    tar_target(fig_intensity_sn, plot_intensity_sn(dat_pilot, condition)),
    tar_target(pl_state_dendrogram, plot_state_dendrogram(dat$parsed, condition))
  )

  raw_examples <- list(
    tar_target(raw_ex_cell1_m31, get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", -31)),
    tar_target(raw_ex_cell1_m13, get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", -13)),
    tar_target(raw_ex_cell1_p6, get_timepoint_raw_data(raw, dat$xyz, "TT206-no_siRNA:1_1-1", 6))
  )
  
  c(
    read_data,
    raw_examples,
    figures,
    figures_per_condition
  ) 
}

time_limits <- c(-50, 50)  # for plots

# Creates initial parameters for selectors Returns a list of two vectors:
# cellcons (cell line/condition) and mcells (movie / cell no)
initial_cellcons <- function(meta) {
  cellcons <- unique(meta$cellcon)
  init_cellcon <- cellcons[1]
  mcells <- meta %>% filter(cellcon == init_cellcon) %>% pull(mcell)
  init_mcell <- mcells[1] 
  list(
    cellcons = cellcons,
    mcells = mcells
  )
}

# Reads Excel files from the folder "data.path", does initial processing of raw
# data and saves the result in the cache file. Does not return anything useful.
reload_data <- function(data.path, sheets, extvol_sheets, cache.file) {
  info <- get_info(data.path)
  dat <- read_cells(info, sheets, extvol_sheets) %>% 
    process_raw_data()
  write_rds(dat, cache.file)
}

# Creates a tibble based on state transition parameters. Used in
# pl_state_distance.
make_state_limit_tb_ <- function(params) {
  pr <- params %>% unlist()
  tibble(
    name = names(pr),
    limit = pr
  ) %>% 
    filter(str_detect(name, "dist.")) %>% 
    mutate(limit = as.numeric(limit)) %>% 
    separate(name, c("what", "state")) %>% 
    mutate(state = factor(state, levels = STATE_COLOUR$state))
}

# Reduce distances a, b, g, r to two distances 1 and 2, just for plotting. Used
# in pl_state_distance.
compact_distances_ <- function(dp) {
  dp %>% 
    mutate(
      dist_1 = if_else(n_dot == 2, dist_a, if_else(n_dot == 3, if_else(dist_r > dist_g, dist_r, dist_g), dist_a)),
      dist_2 = if_else(n_dot == 4, dist_b, as.numeric(NA))
    )
}

# Merge lightblue and blue
merge_blue_ <- function(dp, params) {
  if(params$merge.blue) {
   dp <- dp %>% 
      mutate(
        state = replace(state, params$merge.blue & state == "lightblue", "darkblue"),
        letter = replace(letter, params$merge.blue & letter == "L", "B")
      )
  }
  dp
}

# For a given cell, plot a timeline of distance between dots and their
# corresponding states. For clarity, only the distance on which selection is
# based is shown. The number of dots is encoded by shape and the state is
# encoded with colour. dp is a parsed state table and it must be pre-selected
# for one cell.
pl_state_distance_timeline <- function(dp, params) {
  d <- dp %>% 
    compact_distances_() %>% 
    merge_blue_(params) %>% 
    mutate(dist_max = pmax(dist_1, dist_2, na.rm = TRUE))
  dd <- make_state_limit_tb_(params)
  ds <- drop_na(d)
  ggplot(d, aes(x = time_nebd, y = dist_1, fill = state, shape = factor(n_dot, levels = 1:4))) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "none") +
    geom_hline(data = dd, aes(yintercept = limit, colour = state), linetype = "dotted") +
    geom_segment(aes(xend = time_nebd, yend = 0, y = dist_max), colour = "grey80") +
    geom_point(size = 3, colour = "grey50") +
    scale_fill_manual(values = STATE_COLOUR$colour, drop = FALSE) +
    scale_colour_manual(values = STATE_COLOUR$colour, drop = FALSE) +
    guides(colour = "none") +
    scale_shape_manual(values = c(20, 21, 24, 23), drop = FALSE) +
    geom_point(data =  ds, aes(y = dist_2), shape = 23, size = 3, colour = "grey50") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = seq(-100,100,5)) +
    labs(x = "Time since nebd (min)", y = expression(Distance~(mu * m)), shape = "Num dots", colour = "State")
}

# For a given cell, plot a timeline of every distance stored in the parsed
# table.
pl_all_distance_timeline <- function(dp, params) {
  dp <- dp %>% merge_blue_(params) 
  
  d <- dp %>% 
    select(-c(frame, time, letter, cell_line, condition, cell)) %>% 
    pivot_longer(cols = starts_with("dist"))
  
  ds <- d %>% 
    group_by(cell_id, time_nebd) %>% 
    summarise(d_max = max(value, na.rm = TRUE))
  
  dsum <- d %>% 
    group_by(cell_id, time_nebd) %>% 
    tally() %>% 
    left_join(dp, by = c("time_nebd", "cell_id"))
  
  ggplot(d, aes(x = time_nebd, y = value)) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "none") +
    geom_segment(data = ds, aes(x = time_nebd, xend = time_nebd, y = 0, yend = d_max), colour = "grey70") +
    geom_point(aes(fill = name, colour = name, shape = factor(n_dot, levels = 1:4)), size = 3) +
    scale_fill_manual(values = c("blue", "orange", "green", "red")) +
    scale_colour_manual(values = c("blue", "orange", "green", "red")) +
    scale_shape_manual(values = c(20, 21, 24, 23), drop = FALSE) +
    scale_x_continuous(breaks = seq(-100,100,5)) +
    geom_text(data = dsum, aes(x = time_nebd, y = -0.1, label = letter), vjust = 0, colour = "black") +
    #geom_text(data = dsum, aes(x = time_nebd, y = -0.3, label = n_dot), vjust = -0.6, colour = "black") +
    labs(x = "Time since nebd (min)", y = expression(Distance~(mu * m)), shape = "Num dots", colour = "Distance", fill = "Distance")
}

make_state_map <- function(dp, params) {
  dp %>% 
    merge_blue_(params) %>% 
    mutate(x = time_nebd, y = as_factor(mcell) %>% fct_rev())
}

pl_state_map <- function(dp, params) {
  dp %>% 
    make_state_map(params) %>% 
  ggplot(aes(x = x, y = y, fill = state)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none"
      #axis.text.y = element_blank()
    ) +
    geom_tile() +
    scale_fill_manual(values = STATE_COLOUR$colour, drop = FALSE) +
    scale_x_continuous(breaks = seq(-100, 100, 10), limits = time_limits, expand = c(0,0)) +
    labs(x = "Time since nebd (min)", y = NULL, fill = NULL)
}

make_proportion_map <- function(dp, k, params) {
  dp %>% 
    merge_blue_(params) %>% 
    #mutate(statelet = recode(as.character(state), 
    #  "lightblue" = "blue",
    #  "darkblue" = "blue",
    #  "brown" = "blue"
    #)) %>% 
    filter(state != "none") %>% 
    group_by(time_nebd, state) %>%
    tally() %>% 
    ungroup() %>% 
    # a trick to fill missing data with zeroes
    pivot_wider(id_cols = time_nebd, names_from = state, values_from = n, values_fill = list(n = 0)) %>%
    pivot_longer(-time_nebd, names_to = "state", values_to = "n_state") %>% 
    group_by(time_nebd) %>% 
    mutate(
      n_cells = sum(n_state),
      prop = n_state / n_cells
    ) %>%
    ungroup() %>% 
    mutate(state = factor(state, levels = levels(dp$state))) %>% 
    group_by(state) %>%
    mutate(smooth = runmean(prop, k)) %>% 
    ungroup()
}

pl_proportion_map <- function(dp, k = 5, params) {
  dp %>% 
    make_proportion_map(k, params) %>% 
  ggplot(aes(x = time_nebd, y = smooth, colour = state)) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_line(size = 1.5) +
    scale_colour_manual(values = STATE_COLOUR$colour, drop = FALSE) +
    labs(x = "Time since NEBD (min)", y = "Proportion") +
    scale_x_continuous(breaks = seq(-100, 100, 10), limits = time_limits, expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1))
}


plot_dots <- function(xyz) {
  pl <- xyz %>% 
    plot_ly() %>%
    add_trace(type = "scatter3d", mode = "markers", x = ~x, y = ~y, z = ~z, marker = list(color = ~dot_colour), showlegend = FALSE) %>% 
    layout(font = list(size = 9), scene = list(aspectmode = "data"))
  xyz_red <- xyz %>% filter(dot_colour == "red")
  if(nrow(xyz_red) > 0) {
    pl <- add_paths(pl, x = ~x, y = ~y, z = ~z, data = xyz_red, color = I("red"), showlegend = FALSE)
  }
  xyz_green <- xyz %>% filter(dot_colour == "green")
  if(nrow(xyz_green) > 0) {
    pl <- add_paths(pl, x = ~x, y = ~y, z = ~z, data = xyz_green, color = I("green"), showlegend = FALSE)
  }
  pl
}

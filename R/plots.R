okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

make_time_ticks <- function(val.min=-200, val.max=200, lab.step=5) {
  time_breaks <- seq(val.min, val.max, 1)
  time_labels <- rep("", length(time_breaks))
  idx <- seq(1, length(time_breaks), lab.step)
  time_labels[idx] <- time_breaks[idx]
  list(breaks = time_breaks, labels = time_labels)
}

# plot red vs green intensity and colour identification
# input: one cell from dat_cells
plot_colour_identification <- function(dc) {
  mn <- min(c(dc$intensities$intensity_red, dc$intensities$intensity_green))
  mx <- max(c(dc$intensities$intensity_red, dc$intensities$intensity_green))
  dc$intensities %>% 
    left_join(dc$track_colour, by="track_id") %>% 
  ggplot(aes(x=intensity_green, y=intensity_red, shape=track_id, colour=colour)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_colour_manual(values=c("forestgreen", "red")) +
    geom_abline(slope=1, intercept = 0) +
    geom_point() +
    scale_x_continuous(limits=c(mn, mx)) +
    scale_y_continuous(limits=c(mn, mx))
}

# Look at intensity data in time
# Compare to parsed positional data
# Highlight disparities
plot_colour_timeline <- function(dat, cellid) {
  dc <- dat$celldat[[cellid]]
  dp <- dat$parsed %>% filter(cell_id == cellid)
  
  tcks <- make_time_ticks()
  d <- dc$intensities %>% 
    left_join(dc$track_colour, by="track_id") %>%
    left_join(dc$times, by=c("id", "track_id")) %>% 
    mutate(intensity_diff = intensity_green - intensity_red) %>% 
    arrange(frame, desc(intensity_diff)) %>% 
    group_by(frame) %>% 
    mutate(
      good = identical(colour, sort(colour)) & n() > 1 & !(n() == 2 & first(colour) == last(colour)),
      d_min = min(intensity_diff),
      d_max = max(intensity_diff)
    ) %>% 
    ungroup()
  d_bad <- d %>% filter(!good)
  d_seg <- d %>% select(time_nebd, d_min, d_max) %>% distinct()
  min_d <- min(d$intensity_diff)
  
  dsum <- d %>% 
    group_by(time_nebd) %>% 
    tally() %>% 
    left_join(dp, by="time_nebd") %>% 
    mutate(ff = if_else(n == n_dot, "plain", "bold"))
  
  ggplot() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_hline(yintercept = 0, colour="blue", alpha=0.3, linetype="dashed") +
    geom_vline(data = d_bad, aes(xintercept = time_nebd), colour="grey90", size=3) +
    geom_segment(data = d_seg, aes(x=time_nebd, xend=time_nebd, y=d_min, yend=d_max), colour="grey60") +
    geom_point(data = d, aes(x=time_nebd, y=intensity_diff, shape=track_id, colour=colour)) +
    geom_text(data = dsum, aes(x=time_nebd, y=min_d, label=letter), vjust=0.6) +
    geom_text(data = dsum, aes(x=time_nebd, y=min_d, label=n_dot, fontface=ff), vjust=-0.6) +
    scale_colour_manual(values=c("forestgreen", "red")) +
    scale_x_continuous(breaks = tcks$breaks, labels = tcks$labels) +
    labs(x="Time since nebd (min)", y="Intensity difference (green-red)")
}

animate_cell <- function(d, cl) {
  d %>% 
    filter(cell == cl) %>% 
    mutate(frame = as.integer(frame)) %>% 
  ggplot(aes(x=x, y=y, fill=colour)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    geom_point(shape=21, size=4) +
    scale_fill_manual(values=c("green", "red")) +
    transition_time(frame) +
    labs(title = "{frame_time}")
}

make_state_limit_tb <- function(params) {
  pr <- params %>% unlist()
  tibble(
    name = names(pr),
    limit = pr
  ) %>% 
    filter(str_detect(name, "dist.")) %>% 
    mutate(limit = as.numeric(limit)) %>% 
    separate(name, c("what", "states"), sep="\\.")
}

compact_distances <- function(dp) {
  dp %>% 
    mutate(
      dist_1 = if_else(n_dot==2, dist_a, if_else(n_dot==3, if_else(dist_r > dist_g, dist_r, dist_g), dist_a)),
      dist_2 = if_else(n_dot==4, dist_b, as.numeric(NA))
    )
}


plot_state_distance <- function(dp, params) {
  dp %>% 
    compact_distances() %>% 
    mutate(dist_max = pmax(dist_1, dist_2, na.rm=TRUE)) %>% 
  ggplot(aes(x=time_nebd, y=dist_1, fill=state, shape=factor(n_dot, levels=1:4))) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_hline(data=make_state_limit_tb(params), aes(yintercept = limit), linetype="dotted") +
    geom_segment(aes(xend=time_nebd, yend=0, y=dist_max), colour="grey80") +
    geom_point(size=3, colour="grey50") +
    scale_fill_manual(values=state_colour$colour, drop=FALSE) +
    scale_colour_manual(values=state_colour$colour, drop=FALSE) +
    guides(colour="none") +
    scale_shape_manual(values=c(20, 21, 24, 23), drop=FALSE) +
    geom_point(aes(y=dist_2), shape=23, size=3, colour="grey50") +
    facet_wrap(~cell_id, ncol=1) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
    labs(x="Time since nebd (min)", y=expression(Distance~(mu * m)), shape="Num dots", colour="State")
}

plot_distance_distribution <- function(dp, params, cex=3) {
  dp %>%
    compact_distances() %>% 
    select(condition, cell, n_dot, state, dist_1, dist_2) %>% 
    pivot_longer(cols=c(dist_1, dist_2)) %>%
    drop_na() %>%
    filter(n_dot>1) %>%
  ggplot(aes(x=as_factor(n_dot), y=value, colour=state)) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid = element_blank()
    ) +
    geom_hline(data=make_state_limit_tb(params), aes(yintercept = limit), linetype="dotted") +
    geom_beeswarm(cex=cex) +
    scale_colour_manual(values=state_colour$colour, drop=FALSE) +
    facet_wrap(~condition) +
    labs(x="Number of dots", y="Distance")
}

plot_all_distances <- function(dp, params) {
  d <- dp %>% 
    select(-c(frame, time, letter, condition, cell)) %>% 
    pivot_longer(cols = starts_with("dist"))
  
  ds <- d %>% 
    group_by(cell_id, time_nebd) %>% 
    summarise(d_max = max(value, na.rm=TRUE))
  
  dsum <- d %>% 
    group_by(cell_id, time_nebd) %>% 
    tally() %>% 
    left_join(dp, by=c("time_nebd", "cell_id"))

  ggplot(d, aes(x=time_nebd, y=value)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_segment(data=ds, aes(x=time_nebd, xend=time_nebd, y=0, yend=d_max), colour="grey70") +
    geom_point(aes(fill=name, colour=name, shape=factor(n_dot, levels=1:4))) +
    scale_fill_manual(values = c("blue", "orange", "green", "red")) +
    scale_colour_manual(values = c("blue", "orange", "green", "red")) +
    scale_shape_manual(values=c(20, 21, 24, 23), drop=FALSE) +
    facet_wrap(~cell_id, ncol=1) +
    geom_text(data = dsum, aes(x=time_nebd, y=-0.3, label=letter), vjust=0.6, colour="black") +
    geom_text(data = dsum, aes(x=time_nebd, y=-0.3, label=n_dot), vjust=-0.6, colour="black") +
    labs(x="Time since nebd (min)", y=expression(Distance~(mu * m)), shape="Num dots", colour="Distance", fill="Distance")
}

plot_state_map <- function(dp) {
  dp %>% 
    ggplot(aes(x=time_nebd, y=cell_id, fill=state)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text.y = element_blank()
    ) +
    geom_tile() +
    facet_wrap(~condition, scales="free") +
    scale_fill_manual(values=state_colour$colour, drop=FALSE) +
    labs(x="Time since nebd (min)", y=NULL, fill=NULL)
}


plot_angle_distribution <- function(dp) {
  dp %>%
    filter(!is.na(angle_rg)) %>%
  ggplot(aes(x=state, y=angle_rg * 180 / pi, colour=state)) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_boxplot(outlier.shape = NA) +
    geom_beeswarm() +
    scale_colour_manual(values=state_colour$colour, drop=FALSE) +
    scale_y_continuous(expand=c(0,0), limits=c(0,90), breaks=c(0,30,60,90)) +
    facet_wrap(~condition) +
    labs(x=NULL, y="Angle (deg)")
}

plot_angle_timeline <- function(dp, brks = seq(-50, 50, 10), point.size=1.5, cex=0.8) {
  dp %>%
    filter(!is.na(angle_rg)) %>% 
    mutate(win = cut(time_nebd, breaks=brks)) %>% 
  ggplot(aes(x=win, y=angle_rg * 180 / pi)) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "bottom") +
    geom_boxplot(aes(colour=condition), fill="grey90", outlier.shape = NA, width=0.6) +
    geom_beeswarm(aes(group=condition, fill=state), colour="grey50", shape=21, dodge.width = 0.6, size=point.size, cex=cex) +
    scale_fill_manual(values=state_colour$colour[5:7], drop=TRUE) +
    scale_colour_manual(values=okabe_ito_palette[2:3]) + 
    scale_y_continuous(expand=c(0,0), limits=c(0,90), breaks=c(0,30,60,90)) +
    labs(x="Time window (min)", y="Angle (deg)")
}

plot_distance_angle <- function(dp, params, brks = seq(-50, 50, 10), facet="condition") {
  g <- dp %>%
    mutate(ab = pmax(dist_a, dist_b)) %>%
    mutate(win = cut(time_nebd, breaks=brks)) %>% 
    select(ab, win, angle_ab, angle_rg, state, condition) %>% 
    pivot_longer(c(angle_ab, angle_rg)) %>% 
    drop_na() %>% 
  ggplot(aes(x=ab, y=value * 180 / pi, fill=state)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_point(shape=21, colour="grey50") +
    geom_vline(xintercept = params$dist.red_pink, linetype = "dashed") +
    scale_fill_manual(values=state_colour$colour[5:7], drop=TRUE) +
    scale_y_continuous(expand=c(0,0), limits=c(0,90), breaks=c(0,30,60,90)) +
    labs(x="Max(a, b)", y="Angle (deg)")
  if(facet == "condition") {
    g <- g + facet_grid(name~condition)
  } else {
    g <- g + facet_grid(name~win)
  }
}

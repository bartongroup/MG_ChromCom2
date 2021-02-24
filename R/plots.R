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

plot_colour_timeline <- function(dc) {
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
  d_seg <- d %>% select(time_nedb, d_min, d_max) %>% distinct()
  
  ggplot() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_hline(yintercept = 0, colour="blue", alpha=0.3, linetype="dashed") +
    geom_vline(data = d_bad, aes(xintercept = time_nedb), colour="grey90", size=3) +
    geom_segment(data = d_seg, aes(x=time_nedb, xend=time_nedb, y=d_min, yend=d_max), colour="grey60") +
    geom_point(data = d, aes(x=time_nedb, y=intensity_diff, shape=track_id, colour=colour)) +
    scale_colour_manual(values=c("forestgreen", "red")) +
    scale_x_continuous(breaks = tcks$breaks, labels = tcks$labels) +
    labs(x="Time since NEDB (min)", y="Intensity difference (green-red)")
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

plot_state_distance <- function(dp) {
  dp %>% 
    mutate(dist_max = pmax(dist_1, dist_2, na.rm=TRUE), time_nedb = time_nedb) %>% 
  ggplot(aes(x=time_nedb, y=dist_1, fill=state, shape=factor(n_dot, levels=1:4))) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_hline(data=state_limit_tb, aes(yintercept = limit, colour=state), linetype="dotted") +
    geom_segment(aes(xend=time_nedb, yend=0, y=dist_max), colour="grey80") +
    geom_point(size=3, colour="grey50") +
    scale_fill_manual(values=state_colour$colour, drop=FALSE) +
    scale_colour_manual(values=state_colour$colour, drop=FALSE) +
    guides(colour=FALSE) +
    scale_shape_manual(values=c(20, 21, 24, 23)) +
    geom_point(aes(y=dist_2), shape=23, size=3, colour="grey50") +
    facet_wrap(~cell, ncol=1) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
    labs(x="Time since NEDB (min)", y=expression(Distance~(mu * m)), shape="Num dots", colour="State")
}

plot_distance_distribution <- function(dp, cex=3) {
  dp %>%
    pivot_longer(cols=c(dist_1, dist_2)) %>%
    drop_na() %>%
    filter(n_dot>1) %>%
  ggplot(aes(x=as_factor(n_dot), y=value, colour=state)) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid = element_blank()
    ) +
    geom_hline(data=state_limit_tb, aes(yintercept = limit, colour=state), linetype="dotted") +
    geom_beeswarm(cex=cex) +
    scale_colour_manual(values=state_colour$colour, drop=FALSE) +
    labs(x="Number of dots", y="Distance")
}

plot_state_map <- function(dp) {
  dp %>% 
    ggplot(aes(x=time_nedb, y=cell, fill=state)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text.y = element_blank()
    ) +
    geom_tile() +
    scale_fill_manual(values=state_colour$colour, drop=FALSE) +
    labs(x="Time since NEDB (min)", y=NULL, fill=NULL)
}
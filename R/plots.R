plot_colour_identification <- function(dc) {
  mn <- min(c(dc$intensities$intensity_red, dc$intensities$intensity_green))
  mx <- max(c(dc$intensities$intensity_red, dc$intensities$intensity_green))
  dc$intensities %>% 
    left_join(dc$track_colour, by="track_id") %>% 
  ggplot(aes(x=intensity_green, y=intensity_red, shape=track_id, colour=colour)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_colour_manual(values=c("green", "red")) +
    geom_abline(slope=1, intercept = 0) +
    geom_point() +
    scale_x_continuous(limits=c(mn, mx)) +
    scale_y_continuous(limits=c(mn, mx))
}


animate_cell <- function(d, cl) {
  d %>% 
    filter(cell == cl) %>% 
    mutate(time_point = as.integer(time_point)) %>% 
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
    transition_time(time_point) +
    labs(title = "{frame_time}")
}

plot_state_distance <- function(st) {
  st %>% 
    mutate(dist_max = pmax(dist_1, dist_2, na.rm=TRUE), time = time/60) %>% 
  ggplot(aes(x=time, y=dist_1, fill=state, shape=factor(n_dot, levels=1:4))) +
    geom_segment(aes(xend=time, yend=0, y=dist_max), colour="grey80") +
    geom_point(size=3, colour="grey50") +
    scale_fill_manual(values=state_colour$colour, drop=FALSE) +
    scale_shape_manual(values=c(20, 21, 24, 23)) +
    geom_point(aes(y=dist_2), shape=23, size=3, colour="grey50") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    facet_wrap(~cell, ncol=1) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
    labs(x="Time (min)", y=expression(Distance~(mu * m)), shape="Num dots", colour="State")
}
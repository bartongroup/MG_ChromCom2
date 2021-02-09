plot_colour_identification <- function(d) {
  mn <- min(c(d$intensities$intensity_red, d$intensities$intensity_green))
  mx <- max(c(d$intensities$intensity_red, d$intensities$intensity_green))
  d$intensities %>% 
    left_join(d$track_colour, by="track_id") %>% 
  ggplot(aes(x=intensity_green, y=intensity_red, shape=track_id, colour=colour)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_colour_manual(values=c("green", "red")) +
    geom_abline(slope=1, intercept = 0) +
    geom_point() +
    scale_x_continuous(limits=c(mn, mx)) +
    scale_y_continuous(limits=c(mn, mx))
}
reload_data <- function(data.path, cell_sheets, cache.file) {
  metadata <- get_metadata(data.path)
  dat <- read_cells(metadata, cell_sheets) %>% 
  process_raw_data(with_celldat=FALSE)
  write_rds(dat, cache.file)
}


make_state_limit_tb <- function(params) {
  pr <- params %>% unlist()
  tibble(
    name = names(pr),
    limit = pr
  ) %>% 
    filter(str_detect(name, "dist.")) %>% 
    mutate(limit = as.numeric(limit)) %>% 
    separate(name, c("what", "state")) 
}

pl_state_distance <- function(dp, params) {
  d <- dp %>% 
    mutate(dist_max = pmax(dist_1, dist_2, na.rm=TRUE))
  dd <- make_state_limit_tb(params)
  ds <- drop_na(d)
  ggplot(d, aes(x=time_nebd, y=dist_1, fill=state, shape=factor(n_dot, levels=1:4))) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_hline(data=dd, aes(yintercept = limit, colour=state), linetype="dotted") +
    geom_segment(aes(xend=time_nebd, yend=0, y=dist_max), colour="grey80") +
    geom_point(size=3, colour="grey50") +
    scale_fill_manual(values=state_colour$colour, drop=FALSE) +
    scale_colour_manual(values=state_colour$colour, drop=FALSE) +
    guides(colour=FALSE) +
    scale_shape_manual(values=c(20, 21, 24, 23), drop=FALSE) +
    geom_point(data =  ds, aes(y=dist_2), shape=23, size=3, colour="grey50") +
    scale_y_continuous(expand=expansion(mult=c(0, 0.05))) +
    labs(x="Time since nebd (min)", y=expression(Distance~(mu * m)), shape="Num dots", colour="State")
}

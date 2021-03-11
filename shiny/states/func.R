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
    separate(name, c("what", "state")) %>% 
    mutate(state = factor(state, levels=state_colour$state))
}


compact_distances <- function(dp) {
  dp %>% 
    mutate(
      dist_1 = if_else(n_dot==2, dist_a, if_else(n_dot==3, if_else(dist_r > dist_g, dist_r, dist_g), dist_a)),
      dist_2 = if_else(n_dot==4, dist_b, as.numeric(NA))
    )
}

pl_state_distance <- function(dp, params) {
  d <- dp %>% 
    compact_distances() %>% 
    mutate(dist_max = pmax(dist_1, dist_2, na.rm=TRUE))
  dd <- make_state_limit_tb(params)
  ds <- drop_na(d)
  ggplot(d, aes(x=time_nebd, y=dist_1, fill=state, shape=factor(n_dot, levels=1:4))) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "none") +
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

pl_all_distances <- function(dp, params) {
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
    theme(panel.grid = element_blank(), legend.position="none") +
    geom_segment(data=ds, aes(x=time_nebd, xend=time_nebd, y=0, yend=d_max), colour="grey70") +
    geom_point(aes(fill=name, colour=name, shape=factor(n_dot, levels=1:4)), size=3) +
    scale_fill_manual(values = c("blue", "orange", "green", "red")) +
    scale_colour_manual(values = c("blue", "orange", "green", "red")) +
    scale_shape_manual(values=c(20, 21, 24, 23), drop=FALSE) +
    geom_text(data = dsum, aes(x=time_nebd, y=-0.1, label=letter), vjust=0, colour="black") +
    #geom_text(data = dsum, aes(x=time_nebd, y=-0.3, label=n_dot), vjust=-0.6, colour="black") +
    labs(x="Time since nebd (min)", y=expression(Distance~(mu * m)), shape="Num dots", colour="Distance", fill="Distance")
}

pl_distances <- function(dp, params) {
  g1 <- pl_state_distance(dp, params)
  g2 <- pl_all_distances(dp, params)
  cowplot::plot_grid(g1, g2, align="v", ncol=1)
}


pl_state_map <- function(dp) {
  dp %>% 
    ggplot(aes(x=time_nebd, y=as_factor(cell) %>% fct_rev(), fill=state)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none"
      #axis.text.y = element_blank()
    ) +
    geom_tile() +
    scale_fill_manual(values=state_colour$colour, drop=FALSE) +
    labs(x="Time since nebd (min)", y=NULL, fill=NULL)
}

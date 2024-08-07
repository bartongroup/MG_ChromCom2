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
  ints <- dc$intensities %>% 
    pivot_wider(id_cols=c(id, track_id, frame, time_nebd), names_from=chn_colour, values_from=dots_mean, names_prefix = "intensity_")
  mn <- min(c(ints$intensity_red, ints$intensity_green))
  mx <- max(c(ints$intensity_red, ints$intensity_green))
  ints %>% 
    left_join(dc$track_colour, by="track_id") %>% 
  ggplot(aes(x=intensity_green, y=intensity_red, shape=track_id, colour=dot_colour)) +
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
  dc <- dat$dotsdat[[cellid]]
  dp <- dat$parsed %>% filter(cell_id == cellid)
  
  tcks <- make_time_ticks()
  d <- dc$intensities %>% 
    pivot_wider(id_cols=c(id, track_id, frame), values_from=dots_mean, names_from=chn_colour) %>% 
    left_join(dc$track_colour, by="track_id") %>%
    left_join(dc$times, by=c("id", "track_id", "frame")) %>% 
    mutate(intensity_diff = green - red) %>% 
    arrange(frame, desc(intensity_diff)) %>% 
    group_by(frame) %>% 
    mutate(
      good = identical(dot_colour, sort(dot_colour)) & n() > 1 & !(n() == 2 & first(dot_colour) == last(dot_colour)),
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
    geom_point(data = d, aes(x=time_nebd, y=intensity_diff, shape=track_id, colour=dot_colour)) +
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
    scale_fill_manual(values=STATE_COLOUR$colour, drop=FALSE) +
    scale_colour_manual(values=STATE_COLOUR$colour, drop=FALSE) +
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
    scale_colour_manual(values=STATE_COLOUR$colour, drop=FALSE) +
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
    scale_fill_manual(values=STATE_COLOUR$colour, drop=FALSE) +
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
    scale_colour_manual(values=STATE_COLOUR$colour, drop=FALSE) +
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
    scale_fill_manual(values=STATE_COLOUR$colour[5:7], drop=TRUE) +
    scale_colour_manual(values=okabe_ito_palette[2:4]) + 
    scale_y_continuous(expand=c(0,0), limits=c(0,90), breaks=c(0,30,60,90)) +
    labs(x="Time window (min)", y="Angle (deg)")
}

prepare_angles_ <- function(dp, params, brks = seq(-50, 50, 10), colour="state") {
  dp %>%
    mutate(
      ab = pmax(dist_a, dist_b),
      rg = pmin(dist_r, dist_g),
      win = cut(time_nebd, breaks=brks)
    ) %>% 
    select(ab, rg, win, angle_ab, angle_rg, state, condition) %>% 
    pivot_longer(c(angle_ab, angle_rg)) %>% 
    drop_na() %>% 
    mutate(fll = !!sym(colour))
}

plot_distance_angle <- function(dp, params, brks = seq(-50, 50, 10), facet="condition", colour="state") {
  d <- prepare_angles_(dp, params, brks, colour)
  g <- ggplot(d, aes(x=ab, y=value * 180 / pi, colour=fll)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_point() +
    geom_vline(xintercept = params$dist.red_pink, linetype = "dashed") +
    scale_y_continuous(expand=c(0,0), limits=c(0,90), breaks=c(0,30,60,90)) +
    labs(x="Max(a, b)", y="Angle (deg)")
  
  if(colour == "state") {
    g <- g + scale_colour_manual(values=STATE_COLOUR$colour[5:7], drop=TRUE) + labs(colour="State")
  } else {
    g <- g + scale_colour_viridis_b() + labs(colour="Min(r, g)")
  }
  
  if(facet == "condition") {
    g <- g + facet_grid(name~condition)
  } else {
    g <- g + facet_grid(name~win)
  }
  
  g
}


plot_rg_angle <- function(dp, params, brks = seq(-50, 50, 10), facet="condition") {
  d <- prepare_angles_(dp, params, brks, "state") %>% 
    filter(name == "angle_rg" & ab < params$dist.red_pink)
  g <- ggplot(d, aes(x=rg, y=value * 180 / pi, colour=fll)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_point() +
    scale_y_continuous(expand=c(0,0), limits=c(0,90), breaks=c(0,30,60,90)) +
    labs(x="Min(r, g)", y="Angle red-green (deg)") +
    scale_colour_manual(values=STATE_COLOUR$colour[5:7], drop=TRUE) +
    labs(colour="State", title=glue("Only data with max(a, b) < {params$dist.red_pink}"))
  
  if(facet == "condition") {
    g <- g + facet_grid(name~condition)
  } else {
    g <- g + facet_grid(name~win)
  }
  
  g
}


# Calculate signal-to-noise from intensity data
get_intensity_sn <- function(d) {
  d$intensities %>%
    distinct() %>% 
    filter(dot_colour == chn_colour) %>% 
    left_join(d$metadata, by="cell_id") %>% 
    mutate(
      intensity = dots_max,
      background = extvol_mean,
      SN = intensity / background,
      chn_colour = factor(chn_colour, levels=c("red", "green"))
    )
}

plot_intensity_sn <- function(d, cond) {
  di <- get_intensity_sn(d)
  sn_max <- max(di$SN, na.rm=TRUE)
    
  di %>% 
    filter(condition == cond) %>% 
    #select(mcell, dot_colour, chn_colour, time_nebd, intensity, background, SN) %>% 
    group_split(mcell) %>% 
    map(function(w) {
      g <- ggplot(w) +
        theme_bw() +
        theme(panel.grid = element_blank())
      g1 <- g +
        geom_point(aes(x=time_nebd, y=intensity, colour=dot_colour)) +
        geom_point(aes(x=time_nebd, y=background)) +
        scale_colour_manual(values=c("forestgreen", "red2")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
        facet_wrap(~chn_colour, ncol=1, scales="free_y") +
        labs(x = "Time since NEBD (min)", title = first(w$mcell)) +
        theme(legend.position = "none")
      g2 <- g +
        theme(legend.position = "none") +
        geom_point(aes(x=time_nebd, y=SN, colour=dot_colour)) +
        geom_smooth(aes(x=time_nebd, y=SN), method="loess", se=FALSE, colour="black") +
        facet_wrap(~chn_colour, ncol=1, scales="free_y") +
        scale_colour_manual(values=c("forestgreen", "red2")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, sn_max)) +
        labs(x = "Time since NEBD (min)")
      plot_grid(g1, g2, nrow = 1, align="h")
    }) %>% 
    plot_grid(plotlist = ., ncol=1, align="v")
}

plot_intensity_sn_combined <- function(d) {
  di <- get_intensity_sn(d) %>% 
    left_join(select(d$parsed, c(cell_id, frame, state)), by=c("cell_id", "frame")) %>% 
    mutate(state = replace(state, state == "lightblue", "darkblue"))
  di %>% 
    ggplot(aes(x=time_nebd, y=SN, colour=state)) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point() +
    facet_wrap(~chn_colour) +
    scale_y_continuous(minor_breaks=seq(0,5,0.1), breaks=seq(0, 5, 0.5)) +
    scale_colour_manual(values=STATE_COLOUR$colour, drop=FALSE) +
    labs(x = "Time since NEBD (min)", y="S/N")
}


colourise_dendrogram <- function(dend, group) {
  labs <- group %>% as.character()
  ord <- order.dendrogram(dend)
  ordered_labs <- labs[ord]
  dend %>%
    dendextend::set("labels_col", value=ordered_labs) %>% 
    dendextend::set("leaves_col", value=ordered_labs) %>% 
    dendextend::set("branches_lwd", value=0.2) %>% 
    dendextend::set("leaves_cex", value=1.2) %>% 
    dendextend::set("labels_cex", value=0.8) %>%
    dendextend::set("leaves_pch", value=19)
}

make_correlation_dendrogram <- function(tab) {
  corr.mat <- cor(tab, use="complete.obs")
  dis <- as.dist(1 - corr.mat)  # dissimilarity matrix
  hclust(dis) %>%
    as.dendrogram()
}


plot_state_dendrogram <- function(dp, cond) {
  dfilt <- dp %>% 
    filter(n_dot == 4 & condition == cond)
  M <- dfilt %>%
    mutate(angle_ab = replace_na(angle_ab, 0), angle_rg = replace_na(angle_rg, 0)) %>%
    select(starts_with("dist"), starts_with("angle")) %>%
    as.matrix()
  rownames(M) <- sprintf("%3d : a=%4.2f b=%4.2f ang=%3.0f", dfilt$time_nebd, dfilt$dist_a, dfilt$dist_b, dfilt$angle_rg * 180 / pi)
  M %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    colourise_dendrogram(dfilt$state)
  
  
#    as.ggdend() %>%
#  ggplot(labels=TRUE, horiz=TRUE) +
    #coord_polar(theta="x") +
    #scale_y_reverse(expand = c(0.1, 0)) +
#    ggtitle(cond)
}


plot_intensity_mean_volume <- function(di, cellids=NULL) {
  if(!is.null(cellids)) di <- di %>% filter(cell_id %in% cellids)
  di %>% 
    filter(chn_colour == dot_colour)  %>% 
    mutate(id = row_number()) %>% 
    select(id, dot_colour, cell_id, time_nebd, dots_sum, dots_mean, dots_volume, extvol_sum, extvol_mean, extvol_volume) %>% 
    pivot_longer(-c(id, cell_id, time_nebd, dot_colour), names_sep = "_", names_to=c("from", "stat")) %>% 
    pivot_wider(id_cols=c(id, cell_id, time_nebd, dot_colour, from), names_from=stat, values_from=value) %>% 
  ggplot(aes(x=mean, y=sum / volume, colour = dot_colour)) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_point() +
    facet_grid(cell_id ~ from) +
    scale_colour_manual(values=c("forestgreen", "red2")) +
    geom_smooth(aes(group=1), method="lm", colour="black", se=FALSE)
}   

plot_intensity_sum <- function(di, cellid) {
  di %>%
    filter(cell_id == cellid) %>% 
    filter(chn_colour == dot_colour) %>% 
    mutate(volume = sprintf("%4.2f", dots_volume)) %>% 
  ggplot(aes(x=dots_mean, y=dots_sum, colour=dot_colour, shape=volume)) +
    theme_bw() +
   # theme(legend.position = "none") +
    geom_point() +
    scale_colour_manual(values=c("forestgreen", "red2")) +
    labs(x = "Mean", y = "Sum", title=cellid)
}

plot_mean_sum_n <- function(di) {
  di %>%
    mutate(n = dots_sum / dots_mean) %>%
  ggplot(aes(x=cell_id, y=n)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    geom_beeswarm(cex=0.1) +
    scale_y_log10(breaks=c(1,2,5,10,20,50,100,200,500,1000,2000)) +
    coord_flip() +
    labs(x="N = Sum/Mean", y=NULL)
}


plot_voxel_ratios <- function(vr) {
  vr %>% 
    mutate(
      select = str_remove(cell_id, "TT206-") %>% str_remove(":.+"),
      name = str_remove(name, "dots_")
    ) %>% 
    filter(select != "normal") %>% 
  ggplot(aes(x=time_nebd, y=ratio)) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(colour="grey90", size=0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    geom_hline(yintercept = 1, colour="grey80") +
    geom_segment(aes(xend=time_nebd, yend=1), colour="grey60") +
    geom_point() +
    scale_y_log10(breaks = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)) +
    scale_x_continuous(breaks = 0:20) +
    facet_grid(name ~ select) +
    labs(x="Time (min)", "Ratio to normal")
}
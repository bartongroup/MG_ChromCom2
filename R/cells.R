# ds - data frame with two rows and columns x, y, z
# value - distance between rows
# Correction to z coordinates due to different refraction in oil-based objective
# and water-based medium with cells
dist_xyz <- function(d, z_correction = 0.85) {
  d$z <- d$z * z_correction
  r <- as.matrix(d[, c("x", "y", "z")])
  sqrt(sum((r[2,] - r[1,])^2))
}


# ds - a subset of the main "dat" tibble, needs to contain 2-4 rows
# and co-ordinate columns (x, y, z)
# value - tibble with classification of the state
parse_one_state <- function(ds, dist.lightblue, dist.brown, dist.pink) {
  n_dots <- nrow(ds)

  dst <- NULL
  
  if(n_dots == 1) {
    dst <- 0
    state <- "none"
  } else if(n_dots == 2) {
    dst <- dist_xyz(ds)
    state <- ifelse(dst > dist.lightblue, "lightblue", "black")
    # sometimes we see two dots with the same colour
    clr <- ds$colour
    if(clr[1] == clr[2]) state <- "none"
  } else if(n_dots == 3) {
    dsel <- ds %>% 
      filter(n_colour == 2)
    dst <- dist_xyz(dsel)
    state <- ifelse(dst > dist.brown, "brown", "darkblue")
  } else if(n_dots == 4) {
    # find matching pairs
    ds <- ds %>% arrange(colour)
    dst_a <- dist_xyz(ds[c(1,3), ])
    dst_b <- dist_xyz(ds[c(2,4), ])
    dst_c <- dist_xyz(ds[c(1,4), ])
    dst_d <- dist_xyz(ds[c(2,3), ])
    if(dst_c + dst_d < dst_a + dst_b) {
      dst_a <- dst_c
      dst_b <- dst_d
    }
    state <- ifelse(dst_a < dist.pink & dst_b < dist.pink, "red", "pink")
  }
  
  if(is.null(dst)) {
    dst1 <- dst_a
    dst2 <- dst_b
  } else {
    dst1 <- dst
    dst2 <- as.numeric(NA)
  }
  
  tibble(
    cell_id = ds$cell_id[1],
    frame = ds$frame[1],
    time = ds$time[1],
    time_nebd = ds$time_nebd[1],
    n_dot = n_dots,
    dist_1 = dst1,
    dist_2 = dst2,
    state = state
  )
}

# parse states from xyz data
parse_states <- function(xyz, dist.lightblue = NULL, dist.brown = NULL, dist.pink = NULL) {
  
  if(is.null(dist.lightblue)) dist.lightblue <- state_limit["lightblue"]
  if(is.null(dist.brown)) dist.brown <- state_limit["brown"]
  if(is.null(dist.pink)) dist.pink <- state_limit["pink"]
  
  xyz %>% 
    group_split(cell_id, frame) %>% 
    map_dfr(~parse_one_state(.x, dist.lightblue, dist.brown, dist.pink)) %>% 
    left_join(state_colour %>% select(state, letter), by="state") %>% 
    mutate(
      state = factor(state, levels=state_colour$state),
      letter = factor(letter, levels=state_colour$letter)
    )
}


# convert new data to ChrCom3 object
convert_to_chrcom3 <- function(dat) {
  pars <- c3pars()
  tdat <- dat$parsed %>% 
    mutate(
      letter = as.character(letter)
    ) %>% 
    arrange(time_nebd) %>% 
    pivot_wider(id_cols=cell, names_from=time_nebd, values_from=letter) %>% 
    select(-cell) %>% 
    as.matrix()
  tim <- colnames(tdat) %>% as.numeric()
  
  echr <- ChromCom3(pars, time=tim, cells=tdat, colours=unique(dat$parsed$letter))
}


get_timepoint_raw_data <- function(rw, xyz, cellid, tim) {
  ids <- xyz %>% 
    filter(cell_id == cellid & time_nebd == tim) %>% 
    pull(id)
  sheets <- c("Time", "Position", "Intensity Median Ch=1 Img=1", "Intensity Median Ch=2 Img=1")
  map(sheets, function(sheet) {
    rw$cells[[cellid]][[sheet]] %>% filter(ID %in% ids) %>% mutate(TrackID = as.character(as.integer(TrackID)))
  }) %>% 
    set_names(sheets)
}
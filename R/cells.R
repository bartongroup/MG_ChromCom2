# ds - data frame with two rows and columns x, y, z
# finds distance between rows
dist_xyz <- function(d) {
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
    frame = ds$frame[1],
    time = ds$time[1],
    time_nedb = ds$time_nedb[1],
    n_dot = n_dots,
    dist_1 = dst1,
    dist_2 = dst2,
    state = state,
    cell = ds$cell[1]
  )
}

# parse states from xyz data
parse_states <- function(xyz, dist.lightblue = NULL, dist.brown = NULL, dist.pink = NULL) {
  
  if(is.null(dist.lightblue)) dist.lightblue <- state_limit["lightblue"]
  if(is.null(dist.brown)) dist.brown <- state_limit["brown"]
  if(is.null(dist.pink)) dist.pink <- state_limit["pink"]
  
  xyz %>% 
    group_split(cell, frame) %>% 
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
    arrange(time_nedb) %>% 
    pivot_wider(id_cols=cell, names_from=time_nedb, values_from=letter) %>% 
    select(-cell) %>% 
    as.matrix()
  tim <- colnames(tdat) %>% as.numeric()
  
  echr <- ChromCom3(pars, time=tim, cells=tdat, colours=unique(dat$parsed$letter))
}
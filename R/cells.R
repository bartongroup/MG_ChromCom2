state_colour <- tribble(
  ~state, ~colour,
  "none", "grey70",
  "black", "black",
  "lightblue", "skyblue",
  "darkblue", "mediumblue",
  "brown", "brown",
  "pink", "hotpink",
  "red", "red"
) %>% 
  mutate(state = as_factor(state))



# ds - data frame with two rows and columns x, y, z
# finds distance between rows
dist_xyz <- function(d) {
  r <- as.matrix(d[, c("x", "y", "z")])
  sqrt(sum((r[2,] - r[1,])^2))
}


# ds - a subset of the main "dat" tibble, needs to contain 2-4 rows
# and co-ordinate columns (x, y, z)
# value - tibble with classification of the state
parse_one_state <- function(ds, dist.lightblue = 0.3, dist.brown = 0.75, dist.pink = 0.3) {
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
    dst_red <- dist_xyz(ds %>% filter(colour == "red"))
    dst_green <- dist_xyz(ds %>% filter(colour == "green"))
    state <- ifelse(dst_red < dist.pink & dst_green < dist.pink, "red", "pink")
  }
  
  if(is.null(dst)) {
    dst1 <- dst_red
    dst2 <- dst_green
  } else {
    dst1 <- dst
    dst2 <- as.numeric(NA)
  }
  
  tibble(
    time_point = ds$time_point[1],
    time = ds$time[1],
    n_dot = n_dots,
    dist_1 = dst1,
    dist_2 = dst2,
    state = state,
    cell = ds$cell[1]
  )
}


parse_states <- function(d) {
  d %>% 
    group_split(cell, time_point) %>% 
    map_dfr(~parse_one_state(.x)) %>% 
    mutate(state = factor(state, levels=state_colour$state))
}
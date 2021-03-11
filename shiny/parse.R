
#' Distance between two dots
#'
#' @param d Tibble with two rows and columns x, y, z
#' @param z_correction Correction to z coordinates due to different refraction in oil-based objective
# and water-based medium with cells.
#'
#' @return Distance between the dots.
#' @export
dist_xyz <- function(d, z_correction = 0.85) {
  d$z <- d$z * z_correction
  r <- as.matrix(d[, c("x", "y", "z")])
  sqrt(sum((r[2,] - r[1,])^2))
}


#' Parse one state according to our rules
#'
#' @param ds A subset of the main data tibble, needs to contain 2-4 rows
#' @param params Parsing parameters, a list with dist.lightblue, dist.brown, dist.pink and black.length
#'
#' @return A tibble with parsed data.
#' @export
parse_one_state <- function(ds, params) {
  n_dots <- nrow(ds)
  
  if(n_dots == 1) {
    a <- 0
    b <- 0
    r <- 0
    g <- 0
    state <- "none"
  }
  
  else if(n_dots == 2) {
    d <- dist_xyz(ds)
    # sometimes we see two dots with the same colour
    clr <- ds$colour
    if(clr[1] == clr[2]) {
      if(clr[1] == "red") {
        r <- d
        g <- 0
      } else {
        r <- 0
        g <- d
      }
      a <- 0
      b <- 0
      state <- "none"
    } else {
      a <- d
      b <- d
      r <- 0
      g <- 0
      state <- "lightblue"
    }
  }
  
  else if(n_dots == 3) {
    ds <- ds %>% arrange(n_colour)
    clr <- ds$colour
    a <- dist_xyz(ds[c(1,2),])
    b <- dist_xyz(ds[c(1,3),])
    d <- dist_xyz(ds[c(2,3),])  # these two have the same colour
    if(clr[2] == "red") {
      r <- d
      g <- 0
    } else {
      r <- 0
      g <- d
    }
    state <- ifelse(d > params$dist.brown, "brown", "darkblue")
  }
  
  else if(n_dots == 4) {
    # find matching pairs
    ds <- ds %>% arrange(colour)
    a <- dist_xyz(ds[c(1,3), ])
    b <- dist_xyz(ds[c(2,4), ])
    c <- dist_xyz(ds[c(1,4), ])
    d <- dist_xyz(ds[c(2,3), ])
    g <- dist_xyz(ds[c(1,2), ])  # note: "green" is before "red" when sorted
    r <- dist_xyz(ds[c(3,4), ])
    if(c + d < a + b) {
      a <- c
      b <- d
    }
    state <- ifelse(a < params$dist.pink & b < params$dist.pink, "red", "pink")
  }
  
  tibble(
    cell_id = ds$cell_id[1],
    frame = ds$frame[1],
    time = ds$time[1],
    time_nebd = ds$time_nebd[1],
    n_dot = n_dots,
    dist_a = a,
    dist_b = b,
    dist_r = r,
    dist_g = g,
    state = state
  )
}

#' Parse black state
#'
#' "Black" state is when two dots are closer than a limit "dist.lightblue" for
#' at least "black.length" contiguous time points. Since this require several
#' time points, this state cannot be identified by `parse_one_state` function.
#'
#' @param d The full parsed tibble
#' @param params Parsing parameters
#'
#' @return Input table with some lightblue states turned into black, as per rules.
#' @export
parse_black <- function(d, params) {
  d %>% 
    group_split(cell_id) %>% 
    map_dfr(function(w) {
      possible_black <- w$state == "lightblue" & w$dist_a < params$dist.lightblue
      r <- rle(possible_black)
      tb <- tibble(
        length = r$lengths,
        possible_black = r$values
      ) %>%
        mutate(black = length >= params$black.length & possible_black)
      r$values <- tb$black
      bl <- inverse.rle(r)
      w[bl, "state"] <- "black"
      w[bl, "letter"] <- "K"
      w
    })
  
}

#' Parse cell states from xyz data
#'
#' This function is used internally by `process_parse_raw_data`
#'
#' @param xyz Tibble with xyz coordinates and colours, created by `merge_cell_data`
#' @param params Parsing parameters, a list with dist.lightblue, dist.brown, dist.pink and black.length
#'
#' @return A tibble with parsed states
#' @export
parse_states <- function(xyz, params) {
  xyz %>% 
    group_split(cell_id, frame) %>% 
    map_dfr(~parse_one_state(.x, params)) %>% 
    left_join(state_colour %>% select(state, letter), by="state") %>% 
    mutate(
      state = factor(state, levels=state_colour$state),
      letter = factor(letter, levels=state_colour$letter)
    ) %>% 
    parse_black(params)
}


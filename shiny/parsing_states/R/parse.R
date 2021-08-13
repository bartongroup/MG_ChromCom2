
#' Distance between two dots
#'
#' @param m Matrix with two rows and columns with coordinates x, y, z
#'
#' @return Distance between the dots.
#' @export
dist_xyz <- function(m) {
  sqrt(sum((m[2,] - m[1,])^2))
}


#' Title Cosine of angle between two vectors
#'
#' @param x Vector x
#' @param y Vector y
#'
#' @return Cos angle between x and y
#' @export
cos_angle <- function(x, y){
  as.numeric(x %*% y) / (norm(x, type="2") * norm(y, type="2"))
}

#' Angle between two pairs of dots: red vs green
#'
#' @param m Matrix with four rows and three columns containing x, y, z co-oridnates of four points.
#'
#' @return Angle between vectors from rows 1-2 and 3-4 (in radians)
#' @export
angle_xyz <- function(m) {
  v1 <- m[2, ] - m[1, ]
  v2 <- m[4, ] - m[3, ]
  mu <- cos_angle(v1, v2)
  acos(abs(mu))
}


#' Parse one state according to our rules
#'
#' @param ds A subset of the main data tibble, needs to contain 2-4 rows
#' @param params Parsing parameters - a list
#'
#' @return A tibble with parsed data.
#' @export
parse_one_state <- function(ds, params) {
  n_dots <- nrow(ds)
  angle_ab <- as.numeric(NA)
  angle_rg <- as.numeric(NA)

  if(n_dots == 1) {
    a <- 0
    b <- 0
    r <- 0
    g <- 0
    state <- "none"
  }
  
  else if(n_dots == 2) {
    m <- as.matrix(ds[, c("x", "y", "z")])
    d <- dist_xyz(m)
    # sometimes we see two dots with the same colour
    clr <- ds$dot_colour
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
    ds <- ds[order(ds$n_colour), ]  # much faster than arrange
    clr <- ds$dot_colour
    m <- as.matrix(ds[, c("x", "y", "z")])
    a <- dist_xyz(m[c(1,2),])
    b <- dist_xyz(m[c(1,3),])
    d <- dist_xyz(m[c(2,3),])  # these two have the same colour
    if(clr[2] == "red") {
      r <- d
      g <- 0
    } else {
      r <- 0
      g <- d
    }
    state <- ifelse(d > params$dist.darkblue_brown, "brown", "darkblue")
  }
  
  else if(n_dots == 4) {
    # find matching pairs, data are already arranged by colour
    m <- as.matrix(ds[, c("x", "y", "z")])
    a <- dist_xyz(m[c(1,3), ])
    b <- dist_xyz(m[c(2,4), ])
    c <- dist_xyz(m[c(1,4), ])
    d <- dist_xyz(m[c(2,3), ])
    g <- dist_xyz(m[c(1,2), ])  # note: "green" is before "red" in our factor
    r <- dist_xyz(m[c(3,4), ])
    # looking for a shorter combination
    # after this, the distances are: a[1,3], b[2,4], g[1,2], r[3,4]
    if(c + d < a + b) {
      ds <- ds[c(1,2,4,3), ]  # swap rows 3 and 4
      a <- c
      b <- d
    }
    angle_ab <- angle_xyz(m[c(1, 3, 2, 4), ])
    angle_rg <- angle_xyz(m)
    
    D <- params$dist.red_pink
    A <- params$angle.red_pink * pi / 180
    ang <- angle_rg
    cnd <- eval(str2expression(params$rule.red_pink))
    
    state <- ifelse(cnd, "red", "pink")
    
    # additional rule: if r or g are small, state is brown
    if(g < params$dist.brown_redpink | r < params$dist.brown_redpink) state <- "brown"
  }
  
  # 'c' is much faster than 'tibble' or 'data.frame'. However, it returns a vector of chr, so needs conversion later
  c(
    cell_id = ds$cell_id[1],
    frame = ds$frame[1],
    time = ds$time[1],
    time_nebd = ds$time_nebd[1],
    n_dot = n_dots,
    dist_a = a,
    dist_b = b,
    dist_r = r,
    dist_g = g,
    angle_ab = angle_ab,
    angle_rg = angle_rg,
    state = state
  )
}

#' Parse black state
#'
#' "Black" state is when two dots are closer than a limit "dist.black_lightblue" for
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
      possible_black <- w$state == "lightblue" & w$dist_a < params$dist.black_lightblue
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
#' @param params Parsing parameters, a list with dist.black_lightblue, dist.darkblue_brown, dist.red_pink and black.length
#'
#' @return A tibble with parsed states
#' @export
parse_states <- function(xyz, params) {
  xyz %>% 
    select(cell_id, frame, x, y, z, n_colour, dot_colour, time, time_nebd) %>% 
    arrange(dot_colour) %>% 
    group_split(cell_id, frame) %>% 
    map_dfr(~parse_one_state(.x, params)) %>% 
    mutate_at(vars(frame, time, time_nebd, dist_a, dist_b, dist_r, dist_g, angle_ab, angle_rg), as.numeric) %>%
    left_join(STATE_COLOUR %>% select(state, letter), by="state") %>% 
    mutate(
      state = factor(state, levels=STATE_COLOUR$state),
      letter = factor(letter, levels=STATE_COLOUR$letter)
    ) %>% 
    parse_black(params)
}


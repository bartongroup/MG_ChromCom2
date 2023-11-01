#' Count colours across cells
#'
#' @param cells A matrix with cell colours
#' @param time Time vector
#' @param colours Factor with colours
#'
#' @return Marginal count
cell_count <- function(cells, time, colours) {
  cells |>
    apply(2, function(x) table(factor(x, levels = colours))) |>
    t() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      total = colSums(!is.na(cells)),
      Time = time
    )
}


#' Find onset time points t1, t2 and t3
#'
#' @param pars Model parameters
#' @importFrom stats rexp
#' @return A list with three time points
set_points <- function(pars) {
  t1 <- pars$t0 - rexp(1, 1 / pars$tau1)  # generate random t1 with exponential distribution
  t2base <- ifelse(pars$t2ref == 1, t1, pars$t0)  # reference point for t2 (model switch)
  t2 <- t2base + ifelse(pars$tau2 > 0, rexp(1, 1/pars$tau2), 0)
  t3 <- t1 + ifelse(pars$tau3 > 0, rexp(1, 1/pars$tau3), 0)

  list(
    t1 = t1,
    t2 = t2,
    t3 = t3
  )
}


#' Generate transition times (there are some issues with this)
#'
#' @param pars Parameters of the simulation (t1, dt2, k1, k2)
#'
#' @importFrom stats rexp
#' @return A list with two transition times
transition_times <- function(pars) {
  # find three onset timepoints (t1, t2 and t3)
  tp <- set_points(pars)

  BP <- ifelse(pars$k1 > 0, tp$t1 + rexp(1, pars$k1), 1000)

  t2 <- ifelse(BP > tp$t2, BP, tp$t2)
  PR <- ifelse(pars$k2 > 0, t2 + rexp(1, pars$k2), 1000)

  list(
    BP = BP,
    PR = PR
  )
}


#' Index of a time point
#'
#' @param t Time point in minutes
#' @param tp A list of start, stop and step for the timeline
#' @param maxn Maximum size
#'
#' @return Integer index in the time vector corresponding to t;
time_index <- function(t, tp, maxn = 10000) {
  i <- round((t - tp$start) / tp$step) + 1
  if(i > maxn) i <- maxn
  if(i < 1) i <- 1
  return(i)
}



#' Helper function for \code{tc_simulation}
#'
#' @param state A letter representing current state
#' @param i Current index
#' @param i2 Index corresponding to time t2
#' @param i3 Index corresponding to time t3
#' @param p1 Probability corresponding to rate k1
#' @param p2 Probability corresponding to rate k2
#' @param p3 Probability corresponding to rate k3
#'
#' @return New state
state_transition <- function(state, i, i2, i3, p1, p2, p3) {
  R <- stats::runif(1)
  if(state == "B") {
    if(R < p1) state <- "P"
  }
  else if(state == "P") {
    if(i >= i3 && R < p3)         state <- "B"
    else if (i >= i2 && R < p2)   state <- "R"
  }
  return(state)
}


#' Create a cell using transition mode (there are some issues with this)
#'
#' @param pars Model parameters
#' @param timepars Time parameters
#'
#' @return A cell
tc_transition <- function(pars, timepars) {
  T <- transition_times(pars)

  maxn <- 10000
  iBP <- time_index(T$BP, timepars, maxn)
  iPR <- time_index(T$PR, timepars, maxn)
  #if(iPR <= iBP) iPR = iBP + 1

  # we want to avoid boundary effects
  cell <- rep("B", maxn)
  if(iPR > iBP) cell[(iBP+1):iPR] <- "P"
  cell[(iPR+1):maxn] <- "R"
  cell <- cell[1:timepars$n]

  return(cell)
}

#' Create a cell using simulations mode
#'
#' @param pars Model parameters
#' @param timepars Time parameters
#'
#' @return A cell
tc_simulation <- function(pars, timepars) {
  cell <- rep("-", timepars$n)

  # find three onset timepoints (t1, t2 and t3)
  tp <- set_points(pars)

  # integer indexes in the time table
  i1 <- time_index(tp$t1, timepars)
  i2 <- time_index(tp$t2, timepars)
  i3 <- time_index(tp$t3, timepars)

  # transition probabilities from rates
  p1 <- 1 - exp(-timepars$step * pars$k1)
  p2 <- 1 - exp(-timepars$step * pars$k2)
  p3 <- 1 - exp(-timepars$step * pars$k3)

  cell[1:(i1-1)] <- "B"
  state <- "B"
  for(i in i1:timepars$n) {
    state <- state_transition(state, i, i2, i3, p1, p2, p3)
    cell[i] <- state
  }

  return(cell)
}

#' Generate timeline for one cell
#'
#' @param pars Model parameters
#' @param mode Either "transition" or "simulation"
#' @param timepars A list of start, stop and step for the timeline
#'
#' @return A character vector of colours
timeline_cell <- function(pars, timepars, mode = c("transition", "simulation")) {
  mode <- match.arg(mode)

  if(mode == "transition") {
    cell <- tc_transition(pars, timepars)
  } else if(mode == "simulation") {
    cell <- tc_simulation(pars, timepars)
  }

  return(cell)
}

#' Perform simulation
#'
#' @param chr Initial \code{ChrCom3} object with model parameters.
#' @param ncells Number of cells to generate
#' @param mode Either "transition" or "simulation"
#' @param ncores Number of cores for parallel computation
#'
#' @return A \code{ChrCom3} object with simulation results.
#' @export
generate_cells <- function(chr, ncells = 1000, mode = "simulation", ncores = 7) {
  tc <- parallel::mclapply(1:ncells, function(i) {
    timeline_cell(chr$pars, chr$timepars, mode)
  }, mc.cores = ncores)
  cells <- do.call(rbind, tc)
  cnt <- cell_count(cells, chr$time, chr$colours)
  chr$cells <- cells
  chr$cnt <- cnt
  chr$ncells <- ncells

  return(chr)
}

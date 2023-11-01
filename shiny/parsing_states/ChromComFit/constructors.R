#' \code{chromcom} object constructor
#'
#'
#' \code{chromcom} will create an empty object with model parameters. Optionally, data can be added.
#'
#' @param pars A list of model parameters
#' @param time An optional time vector
#' @param cells An optional matrix with data
#' @param timepars Parameters for the time sequence
#' @param colours Colour names
#'
#' @return A \code{chromcom} object.
#' @import assertthat
#' @importFrom methods is
#' @export
chromcom <- function(pars, time = NULL, cells = NULL, timepars = list(start = -140, stop = 90, step = 1),
                     colours = c("B", "P", "R")) {
  assert_that(is(pars, "cpars"))
  if(is.null(time)) {
    start <- timepars$start
    stop <- timepars$stop
    step <- timepars$step
    time = seq(from = start, to = stop, by = step)
  } else {
    start <- time[1]
    stop <- time[length(time)]
    step <- time[2] - time[1]
  }
  n <- length(time)

  if(is.null(cells)) {
    cnt <- NULL
  } else {
    if(is.null(time)) stop("Need time")
    cnt <- cell_count(cells, time, colours)
  }

  obj <- list(
    colours = colours,
    timepars = list(
      start = start,
      stop = stop,
      step = step,
      n = n
    ),
    time = time,
    pars = pars,
    cells = cells,
    cnt = cnt
  )
  class(obj) <- append(class(obj), "chromcom")
  return(obj)
}


#' \code{cpars} object constructor
#'
#' @param t0 Nuclear envelope breakdown time (should be zero)
#' @param tau1 Timescale for onset of B->P transition
#' @param tau2 Timescale for delay of P->R transition (zero if no delay)
#' @param tau3 Timescale for delay of P->B transition (zero if no delay)
#' @param k1 B->P rate
#' @param k2 P->R rate
#' @param k3 R->B rate
#' @param t2ref Reference point for time t2, either 1 for "t1" or 0 for "t0"
#' @param squeeze Whether to squeeze
#' @param dummy Logical, if set empty list is returned
#'
#' @return Model parameters object
#' @export
cpars <- function(
    t0 = 0,
    tau1 = 20,
    tau2 = 0,
    tau3 = 0,
    k1 = 0.04,
    k2 = 0.04,
    k3 = 0,
    t2ref = 1,
    squeeze = 0,
    dummy = FALSE
) {
  if(dummy) {
    pars = list()
  } else {
    pars <- list(t0 = t0, tau1 = tau1, tau2 = tau2, tau3 = tau3, k1 = k1, k2 = k2, k3 = k3, t2ref = t2ref, squeeze = squeeze)
  }
  class(pars) <- append(class(pars), "cpars")
  return(pars)
}

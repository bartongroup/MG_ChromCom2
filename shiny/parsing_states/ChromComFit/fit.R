# Old version
oe_error_ <- function(chr, echr, time_limits, sel = NULL) {

  get_prop <- function(ch, col) {
    p <- stats::ts(ch$cnt[[col]] / ch$cnt$total, start = ch$timepars$start, deltat = ch$timepars$step)
    p[which(is.nan(p) | is.infinite(p))] <- NA
    #p <- window(p, start=limits[1], end=limits[2])
    #p <- squeeze(p, ch$pars$squeeze)
    p
  }

  if(is.null(sel)) {
    # find the length within window (only for the purpose of sel)
    # not very elegant, but not used by the fitting sub
    sel <- which(echr$time >= time_limits[1] & echr$time <= time_limits[2])
  }

  rms <- 0
  for(col in chr$colours) {
    xo <- get_prop(echr, col)
    xe <- get_prop(chr, col)
    rms <- rms + sqrt(sum((xo[sel] - xe[sel])^2, na.rm = TRUE))
  }
  return(rms)
}

#' Data-model error
#'
#' Using test of goodness of fit between observed and model counts in the limits
#' window.
#'
#' @param mod Model data: proportions
#' @param dat Experimental data: counts and errors of proportion
#' @param sel An integer vector with selection of points for bootstrapping
#'
#' @return RMS over all three curves
#' @export
oe_error <- function(mod, dat, sel) {
  prop <- model_prop <- lo <- up <- err <- sr <- NULL

  d <- dat |>
    dtplyr::lazy_dt() |>
    dplyr::left_join(mod, by = c("Time", "colour")) |>
    # model = observed, remove as it can lead to NaN
    dplyr::filter(abs(prop - model_prop) > 1e-6) |>
    # find error on the side of model
    dplyr::mutate(err = dplyr::if_else(prop > model_prop, prop - lo, up - prop)) |>
    dplyr::mutate(sr = ((prop - model_prop) / err)^2) |>
    dplyr::summarise(ssr = sum(sr)) |>
    tibble::as_tibble()

  ss <- d$ssr
  return(ss)
}



#' Error function to minimise by optim
#'
#' @param p A numeric vector of parameters
#' @param pars A \code{cpars} object, full parameters.
#' @param dat Data to fit.
#' @param timepars Time parameters for data.
#' @param ncells Number of cells to generate.
#' @param time_limits Time limits
#' @param mode How to generate timelines. Either "simulation" or "transition"
#' @param sel An integer vector with selection of points for bootstrapping
#' @param ncores Number of cores to use
#'
#' @return rms error
error_fun <- function(p, pars, dat, timepars, ncells, time_limits, mode, sel, ncores = 7) {
  p |>
    vector_par(pars) |>
    chromcom(timepars = timepars) |>
    generate_cells(ncells = ncells, mode = mode, ncores = ncores) |>
    get_model_proportion() |>
    oe_error(dat, sel)
}



#' Fit ChromCom data with a model
#'
#' Fits data with a model in which \code{freepars} are free.
#' @param echr A \code{chromcom} object with experimental data
#' @param pars A \code{cpars} object with initial parameters
#' @param freepars A character vector with names of free parameters
#' @param ncells Number of cells to simulate
#' @param ntry Number of tries in search
#' @param ncores Number of cores
#' @param time_limits A two-element vector with fit limits (in minutes)
#' @param mode How to generate timelines. Either "simulation" or "transition"
#' @param bootstrap Logical. If TRUE, fit will be done on a random sample with replacement of time points
#' @param par_lower Named vector; lower limits on parameters
#' @param par_upper Named vector; upper limits on parameters
#'
#' @return A \code{chromcom} object with the best-fitting model. "rms" field
#'   is added. \code{\link{optim}} function is used for minimization with method "L-BFGS-B".
#' @import assertthat
#' @export
fit_chr <- function(echr, pars, freepars, ncells = 1000, ntry = 10, ncores = 7,
                    time_limits = c(-50, 30), mode = "simulation", bootstrap = FALSE,
                    par_lower = c(t0 = -50, tau1 = 3, k1 = 0, k2 = 0, k3 = 0, tau2 = 3, tau3= 3, squeeze = 0),
                    par_upper = c(t0 = 50, tau1 = 50, k1 = 0.2, k2 = 0.2, k3 = 0.2, tau2 = 50, tau3 = 50, squeeze = 0.4)) {
  # Binding variables from non-standard evaluation locally
  value <- NULL

  assert_that(is(pars, "cpars"))
  assert_that(is(echr, "chromcom"))

  dat <- get_proportion_errors(echr)

  p <- par_vector(pars, freepars)
  lower <- par_lower[freepars]
  upper <- par_upper[freepars]

  sel <- echr$time[echr$time >= time_limits[1] & echr$time <= time_limits[2]]
  # find bootstrap selection based on the window
  if(bootstrap) {
    sel <- sample(sel, length(sel), replace=TRUE)
    print(sel)
  }

  df <- purrr::map_dfr(1:ntry, function(i) {
    print(paste("Try", i))
    opt <- stats::optim(p, error_fun, gr = NULL, pars, dat, echr$timepars, ncells, time_limits,
                        mode, sel, ncores, method="L-BFGS-B", lower = lower, upper = upper,
                        control = list(trace = 3))

    tibble::tibble(
        i = i,
        value = opt$value,
        convergence = opt$convergence,
        pars = list(opt$par)
      )
  })

  best_fit <- df |>
    dplyr::arrange(value) |>
    dplyr::slice(1)

  pars <- vector_par(unlist(best_fit$pars), pars)
  chr <- chromcom(pars, timepars = echr$timepars)
  chr <- generate_cells(chr, ncells = ncells, mode = mode)
  chr$chi2 <- best_fit$value
  return(chr)
}


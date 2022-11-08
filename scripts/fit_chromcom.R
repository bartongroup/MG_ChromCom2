#!/usr/bin/env Rscript

VERSION <- "1.0.0"
cat(paste("\n  fit_chromcom version", VERSION, "\n\n"))

suppressPackageStartupMessages({
  library(optparse)
  library(tibble)
  library(tidyr)
  library(readr)
  library(ChromComFit)
})
  
options(dplyr.summarise.inform = FALSE)

RED_PINK_RULES <- c(
  "(a < D & b < D) | (ang < A & (a < D | b < D))",
  "(a < D & b < D) & ang < A",
  "(a < D | b < D) & ang < A"
)

DEFAULT_PARAMS <- list(
  dist.black_lightblue = 0.4,
  dist.darkblue_brown = 0.75,
  dist.red_pink = 0.5,
  dist.brown_redpink = 0.5,
  black.length = 5,
  angle.red_pink = 30,
  rule.red_pink = RED_PINK_RULES[1]
)

# parameters used in the parser
PARSER_PARAMS <- list(
  dist.black_lightblue = 0,
  dist.darkblue_brown = 0.75,
  dist.red_pink = 0.4,
  dist.brown_redpink = 0.2,
  black.length = 5,
  angle.red_pink = 45,
  rule.red_pink = RED_PINK_RULES[1]
)


###################################################################

option_list <- list(
  make_option(
    opt_str = c("-i", "--input-file"),
    action = "store",
    default = NA,
    type = "character",
    help = "Input TSV file with parsed data, created by 'prepare_chromcom.R'."
  ),
  make_option(
    opt_str = c("-o", "--output-file"),
    action = "store",
    default = NA,
    type = "character",
    help = "Output RDS file."
  ),
  make_option(
    opt_str = c("--t0"),
    action = "store",
    default = 0,
    type = "double",
    help = "Nuclear envelope breakdown time (fixed) [default %default]."
  ),
  make_option(
    opt_str = c("--tau1"),
    action = "store",
    default = 15,
    type = "double",
    help = "Time t1, B→P occurs after t0 − t1 with rate k1 [default %default]."
  ),
  make_option(
    opt_str = c("--tau2"),
    action = "store",
    default = 8,
    type = "double",
    help = "Time t2, P→R occurs after t0 − t1 + t2 with rate k1 [default %default]."
  ),
  make_option(
    opt_str = c("--k1"),
    action = "store",
    default = 0.05,
    type = "double",
    help = "Rate k1, B→P occurs after t0 − t1 with rate k1 [default %default]."
  ),
  make_option(
    opt_str = c("--k2"),
    action = "store",
    default = 0.06,
    type = "double",
    help = "Rate k2, P→R occurs after t0 − t1 + t2 with rate k1 [default %default]."
  ),
  make_option(
    opt_str = c("--timelo"),
    action = "store",
    default = -30,
    type = "double",
    help = "Lower bound on time limits [default %default]."
  ),
  make_option(
    opt_str = c("--timeup"),
    action = "store",
    default = 30,
    type = "double",
    help = "Upper bound on time limits [default %default]."
  ),
  make_option(
    opt_str = c("--ncells"),
    action = "store",
    default = 1000,
    type = "integer",
    help = "Number of cells for simulation fitting [default %default]."
  ),
  make_option(
    opt_str = c("--ntry"),
    action = "store",
    default = 30,
    type = "integer",
    help = "Number of tries in simulation fitting [default %default]."
  ),
  make_option(
    opt_str = c("--ncores"),
    action = "store",
    default = 12,
    type = "integer",
    help = "Number of cores [default %default]."
  ),
  make_option(
    opt_str = c("-b", "--bootstrap"),
    action = "store_true",
    default = FALSE,
    type = "logical",
    help = "Run in bootstrap mode to evaluate parameter uncertainties. The output will be a TSV file with one row of best-fitting parametrs."
  )
)


op <- OptionParser(option_list = option_list)
opt <- parse_args(op)

input_file <- opt$`input-file`
output_file <- opt$`output-file`


if(is.na(input_file) | is.na(output_file)) {
  print_help(op)
  stop("Option(s) missing")
}

stopifnot(file.exists(input_file))


###########################################################

parsed <- readr::read_tsv(input_file, show_col_types = FALSE)
echr <- parsed |> 
  ChromComFit::parsed_to_chrcom3()

fitpars <- ChromComFit::cpars(
  t0 = opt$t0,
  tau1 = opt$tau1,
  tau2 = opt$tau2,
  k1 = opt$k1,
  k2 = opt$k2,
  t2ref = 1
)
free <- c("tau1", "k1", "k2", "tau2")

fit <- ChromComFit::fit_chr(echr, fitpars, free, ncells = opt$ncells, ntry = opt$ntry, bootstrap = opt$bootstrap,
                            ncores = opt$ncores, time_limits = c(opt$timelo, opt$timeup))

if (opt$bootstrap) {
  fit$pars |>
    unlist() |>
    tibble::enframe() |>
    tidyr::pivot_wider() |> 
    readr::write_tsv(output_file)
} else {
  list(
    fit = fit,
    echr = echr,
    parsed = parsed,
    parser_params = PARSER_PARAMS,
    starting_fitpars = fitpars
  ) |> 
    readr::write_rds(output_file)
}


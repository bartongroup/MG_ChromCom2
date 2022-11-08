#!/usr/bin/env Rscript

VERSION <- "1.0.0"
cat(paste("\n  prepare_chromcom version", VERSION, "\n\n"))

suppressPackageStartupMessages({
  library(optparse)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(ChromComFit)
  library(ChromComParse)
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

CELL_SHEETS <- c(
  "Intensity Sum Ch=1 Img=1",
  "Intensity Sum Ch=2 Img=1",
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Max Ch=1 Img=1",
  "Intensity Max Ch=2 Img=1",
  "Volume",
  "Time",
  "Position"
)

EXTVOL_SHEETS <- c(
  "Intensity Sum Ch=1 Img=1",
  "Intensity Sum Ch=2 Img=1",
  "Intensity Min Ch=1 Img=1",
  "Intensity Min Ch=2 Img=1",
  "Intensity Mean Ch=1 Img=1",
  "Intensity Mean Ch=2 Img=1",
  "Intensity Median Ch=1 Img=1",
  "Intensity Median Ch=2 Img=1",
  "Volume",
  "Time"
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
    opt_str = c("-d", "--data-dir"),
    action = "store",
    default = NA,
    type = "character",
    help = "Directory with input Excel files, data from the instrument and info files."
  ),
  make_option(
    opt_str = c("-o", "--output-file"),
    action = "store",
    default = NA,
    type = "character",
    help = "Output TSV file."
  ),
  make_option(
    opt_str = c("-l", "--cell-line"),
    action = "store",
    default = NA,
    type = "character",
    help = "Cell line to select from input data, e.g. TT204."
  ),
  make_option(
    opt_str = c("-c", "--condition"),
    action = "store",
    default = NA,
    type = "character",
    help = "Condition to select from input data, e.g. Thy_NCAPD2."
  )
)


op <- OptionParser(option_list = option_list)
opt <- parse_args(op)

data_dir <- opt$`data-dir`
sel_cell_line <- opt$`cell-line`
sel_condition <- opt$`condition`
out_file <- opt$`output-file`


if(is.na(data_dir) | is.na(sel_cell_line) | is.na(sel_condition) | is.na(out_file)) {
  print_help(op)
  stop("Option(s) missing")
}

stopifnot(dir.exists(data_dir))


###########################################################

# Read data
full_info <- ChromComParse::get_info(data_dir)
info <- full_info
info$metadata <- full_info$metadata |> 
  dplyr::filter(condition == sel_condition & cell_line == sel_cell_line)

parsed <- info |> 
  ChromComParse::read_cells(CELL_SHEETS, EXTVOL_SHEETS, verbose = FALSE) |> 
  ChromComParse::process_raw_data() |> 
  ChromComParse::parse_xyz_data(PARSER_PARAMS) |>
  purrr::pluck("parsed")

readr::write_tsv(parsed, out_file)

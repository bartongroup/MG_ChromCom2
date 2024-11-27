info_cols <- tibble::tribble(
  ~raw_name, ~name, ~type,
  "file name", "name", "text",
  "frame NEBD", "nebd_frame", "numeric",
  "cell line", "cell_line", "text",
  "condition", "condition", "text",
  "movie no.", "movie", "text",
  "cell no.", "cell", "numeric",
  "pair no.", "pair", "numeric",
  "experiment date", "date", "date",
  "rupture", "rupture", "text",
  "frame nuclear rupture", "rupture_frame", "numeric"
)

track_cols <- tibble::tribble(
  ~raw_name, ~name, ~type,
  "file name", "name", "text",
  "track id", "track_id", "text",
  "colour", "dot_colour", "text"
)


#' Get info
#'
#' Create info. Needs *info.xlsx files for each cell.
#'
#' @param path Directory containing *info.xlsx files.
#' @param colour_conversion A named vector with alternative colour names
#'
#' @return A list with two elements: metadata - tibble with cell name, id,
#'   condition, cell_id, nebd_frame and Excel file names necessary to read all
#'   data; track_colour - data about track colours
#' @export
get_info <- function(path, colour_conversion = c("red" = "red", "green" = "green")) {
  files <- list.files(path, pattern = "*info\\.xlsx$", full.names = TRUE, recursive = TRUE)

  # Check if sheets OK
  for (fn in files) {
    sh <- readxl::excel_sheets(fn)
    if (!(sh[1] == "metadata" & sh[2] == "trackid")) stop(str_str_glue("Sheets in {fn} should be 'metadata' and 'trackid'."))
  }

  meta <- purrr::map(files, function(fn) {
    rex <- tryCatch(
      readxl::read_excel(fn,
        sheet = "metadata",
        col_names = TRUE,
        progress = FALSE
      ),
      error = function(err) {
        message(stringr::str_glue("Error while reading {fn}"))
        stop("Dead")
      },
      warning = function(err) {
        message(stringr::str_glue("Warning while reading {fn}"))
        stop("Dead")
      }
    )

    # Older name was 'frame rupture'
    if("frame rupture" %in% colnames(rex))
      rex <- rex |> dplyr::rename(`frame nuclear rupture` = `frame rupture`)

    n2n <- rlang::set_names(info_cols$raw_name, info_cols$name)

    # Older infos do not have rupture and pair columns
    missing_cols <- setdiff(n2n, names(rex))
    if(length(missing_cols) > 0)
      rex[, missing_cols] <- NA_character_
    # Where present, `pair. no` is read as numeric, but we need character
    rex$`pair no.` <- as.character(rex$`pair no.`)

    rex |>
      dplyr::rename(all_of(n2n)) |>
      dplyr::select(all_of(names(n2n))) |>
      dplyr::mutate(
        rupture_frame = as.integer(rupture_frame),
        path = dirname(fn),
        info_file = fn
      )
  }) |>
    list_rbind() |> 
    dplyr::mutate(
      pair = tidyr::replace_na(pair, "1"),
      pair = stringr::str_glue("p{pair}"),
      nebd_frame = as.integer(nebd_frame),
      cell_line = stringr::str_replace_all(cell_line, " ", "_"),
      condition = stringr::str_replace_all(condition, " ", "_"),
      date = as.Date(date)
   ) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      day = date |> as.character() |> forcats::as_factor() |> as.integer(),
      day = sprintf("%02d", day)
    ) |>
    tidyr::unite(cellcon, c(cell_line, condition), sep = "-", remove = FALSE) |>
    tidyr::unite(mcell, c(day, movie, cell, pair), sep = "-", remove = FALSE) |>
    tidyr::unite(cell_id, c(cellcon, mcell), remove = FALSE, sep = ":") |>
    dplyr::mutate(
      cell_file = file.path(path, stringr::str_glue("{name}.xls")),
      background_file = file.path(path, stringr::str_glue("{name}_extendedvol.xls"))
    ) |>
    dplyr::mutate(across(c(name, cell_line, condition, cellcon, movie, mcell), forcats::as_factor))

  t2n <- rlang::set_names(track_cols$raw_name, track_cols$name)
  trcol <- purrr::map(meta$info_file, function(file) {
    readxl::read_excel(file, sheet = "trackid") |>
    dplyr::rename(all_of(t2n)) |>
    dplyr::mutate(
      track_id = as.character(as.integer(track_id)),
      dot_colour = as.character(dot_colour)
    ) |>
    dplyr::mutate(dot_colour = colour_conversion[dot_colour] |> unname())
  }) |>
    purrr::list_rbind() |>
    dplyr::left_join(dplyr::select(meta, name, cell_id), by = "name")

  # check for duplicates
  dm <- meta |> dplyr::add_count(cell_id) |> dplyr::filter(n > 1)
  if (nrow(dm) > 0) {
    print(dm |> dplyr::select(name, cell_id, n) |> dplyr::distinct())
    stop("Duplicates detected")
  }
  tm <- trcol |> dplyr::add_count(cell_id, track_id) |> dplyr::filter(n > 1)
  if (nrow(tm) > 0) {
    print(tm)
    stop("Duplicated trackID detected")
  }

  list(
    metadata = meta,
    track_colours = trcol
  )
}


#' Read selected sheets from an Excel file.
#'
#' @param excel_file Input file. Warning: Imaris software writes an xls file
#'   which is incompatible with readxl. You need to open each Excel file in
#'   Excel and save it again.
#' @param sheets Sheets to read.
#' @param verbose Logical, print progress.
#'
#' @return A named list with sheets.
read_excel_cell <- function(excel_file, sheets = NULL, verbose = TRUE) {
  if (!file.exists(excel_file)) return(NULL)
  if (verbose) cat(stringr::str_glue("\nReading {excel_file}\n\n"))
  if (is.null(sheets)) {
    sheets <- readxl::excel_sheets(path.expand(excel_file))  # there is a bug in readxl that doesn't take relative paths
  }
  suppressMessages({
    purrr::map(sheets, ~readxl::read_excel(path.expand(excel_file), .x, skip = 1)) |>
      rlang::set_names(sheets)
  })
}


#' Test if files exist
#'
#' Check if all files specified in metadata exist.
#'
#' @param meta Metadata tibble.
#'
#' @return A test
test_for_files <- function(meta) {
  m <- meta |>
    dplyr::select(cell_file, info_file) |>
    tidyr::pivot_longer(cols = everything(), names_to = "type", values_to = "file_name")
  for (i in 1:nrow(m)) {
    r <- m[i, ]
    if (!file.exists(r$file_name)) {
      stop(stringr::str_glue("File {r$file_name} not found."))
    }
  }
}



#' Read all cell data
#'
#' Read cell data and track colour data from Excel files.
#'
#' @param info Info tibble
#' @param sheets A vector with sheet names to be read from the main cell file.
#' @param extvol_sheets A vector with sheet names for background (extendedvol) files.
#' @param verbose If TRUE progess is printed
#'
#' @return A list with metadata, cells and track_colours.
#' @export
read_cells <- function(info, sheets, extvol_sheets, verbose = TRUE) {
  test_for_files(info$metadata)
  dots <- purrr::map(info$metadata$cell_file, ~read_excel_cell(.x, sheets, verbose = verbose)) |>
    rlang::set_names(info$metadata$cell_id)
  extvol <- purrr::map(info$metadata$background_file, ~read_excel_cell(.x, extvol_sheets, verbose = verbose)) |>
    rlang::set_names(info$metadata$cell_id)
  list(
    metadata = info$metadata,
    track_colours = info$track_colours,
    dots = dots,
    extvol = extvol
  )
}


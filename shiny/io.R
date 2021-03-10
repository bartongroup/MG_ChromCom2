#' Get metadata
#'
#' Create metadata. Needs *info.xlsx files for each cell.
#'
#' @param path Directory containing *info.xlsx files.
#'
#' @return Tibble with cell name, id, condition, cell_id, nebd_frame and
#' Excel file names necessary to read all data
#' @export
get_metadata <- function(path) {
  files <- dir(path, pattern="*info.xlsx", full.names=FALSE)
  map_dfr(files, function(f) {
    d <- read_excel(file.path(path, f))
  }) %>% 
    set_names(c("name", "nebd_frame", "condition", "cell")) %>% 
    unite("cell_id", c(condition, cell), remove=FALSE, sep="_") %>% 
    mutate(
      cell_file = file.path(path, glue("{name}.xlsx")),
      trackid_file = file.path(path, glue("{name}_trackid.xlsx")),
      info_file = file.path(path, glue("{name}_info.xlsx"))
    ) %>% 
    mutate_at(vars(name, condition), as_factor)
}


#' Read selected sheets from an Excel file.
#'
#' @param excel_file Input file. Warning: it needs to be xlsx file. Does not
#'   work with some older versions of xls files, in particular Excel output from
#'   Imaris software. Hence, each cell file has to be manually converted into
#'   xlsx.
#' @param sheets Sheets to read.
#' @param verbose Logical, print progress.
#'
#' @return A named list with sheets.
#' @export
read_excel_cell <- function(excel_file, sheets, verbose=TRUE) {
  stopifnot(file.exists(excel_file))
  if(verbose) cat(glue("\nReading {excel_file}\n\n"))
  map(sheets, ~read_excel(excel_file, .x, skip=1)) %>% set_names(sheets)
}


#' Test if files exist
#'
#' Check if all files specified in metadata exist.
#'
#' @param meta Metadata tibble.
#'
#' @return
#' @export
test_for_files <- function(meta) {
  m <- meta %>% 
    select(cell_file, trackid_file, info_file) %>% 
    pivot_longer(cols = everything(), names_to = "type", values_to = "file_name")
  for(i in 1:nrow(m)) {
    r <- m[i, ]
    if(!file.exists(r$file_name)) {
      stop(glue("File {r$file_name} not found."))
    }
  }
}



#' Read all cell data
#'
#' Read cell data and track colour data from Excel files.
#'
#' @param meta Metadata tibble
#' @param sheets A vector with sheet names to be read from the main cell file.
#'
#' @return A list with metadata, cells and track_colours.
#' @export
read_cells <- function(meta, sheets) {
  test_for_files(meta)
  cls <- map(meta$cell_file, ~read_excel_cell(.x, sheets)) %>%
    set_names(meta$cell_id)
  trcol <- map_dfr(meta$trackid_file, ~read_excel(.x)) %>%
    set_names(c("name", "track_id", "colour")) %>%
    mutate(track_id = as.character(as.integer(track_id))) %>% 
    left_join(select(meta, name, cell_id), by="name")
  list(
    metadata = meta,
    cells = cls,
    track_colours = trcol
  )
}


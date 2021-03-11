#' Get info
#'
#' Create info. Needs *info.xlsx files for each cell.
#'
#' @param path Directory containing *info.xlsx files.
#'
#' @return A list with two elements: metadata - tibble with cell name, id,
#'   condition, cell_id, nebd_frame and Excel file names necessary to read all
#'   data; track_colour - data about track colours
#' @export
get_info <- function(path) {
  files <- dir(path, pattern="*info.xlsx", full.names=FALSE)
  
  # Check if sheets OK
  for(f in files) {
    fn <- file.path(path, f)
    sh <- excel_sheets(fn)
    if(!(sh[1] == "metadata" & sh[2] == "trackid")) stop(glue("Sheets in {fn} should be 'metadata' and 'trackid'."))
  }
  
  meta <- map_dfr(files, function(f) {
    fn <- file.path(path, f)
    read_excel(fn, sheet="metadata") %>% 
    set_names(c("name", "nebd_frame", "condition", "cell", "date")) %>% 
    unite("cell_id", c(condition, cell), remove=FALSE, sep="_") %>% 
    mutate(
      cell_file = file.path(path, glue("{name}.xls")),
      info_file = fn
    ) %>% 
    mutate_at(vars(name, condition), as_factor) %>% 
    mutate(date = as.Date(date))
  })
  
  trcol <- map_dfr(meta$info_file, ~read_excel(.x, sheet="trackid")) %>%
    set_names(c("name", "track_id", "colour")) %>%
    mutate(track_id = as.character(as.integer(track_id))) %>% 
    left_join(select(meta, name, cell_id), by="name")
  
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
    select(cell_file, info_file) %>% 
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
read_cells <- function(info, sheets) {
  test_for_files(info$metadata)
  cls <- map(info$metadata$cell_file, ~read_excel_cell(.x, sheets)) %>%
    set_names(info$metadata$cell_id)
  list(
    metadata = info$metadata,
    track_colours = info$track_colours,
    cells = cls
  )
}


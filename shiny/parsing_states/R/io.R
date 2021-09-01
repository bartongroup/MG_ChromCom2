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
  files <- list.files(path, pattern = "*info.xlsx", full.names = TRUE, recursive = TRUE)
  
  # Check if sheets OK
  for(fn in files) {
    sh <- readxl::excel_sheets(fn)
    if(!(sh[1] == "metadata" & sh[2] == "trackid")) stop(glue("Sheets in {fn} should be 'metadata' and 'trackid'."))
  }
  
  meta <- map_dfr(files, function(fn) {
    fpath <- dirname(fn)
    r <- readxl::read_excel(fn,
      sheet = "metadata",
      col_names = c("name", "nebd_frame", "cell_line", "condition", "movie", "cell", "date"),
      col_types = c("text", "numeric", "text", "text", "text", "numeric", "date"),
      skip = 1
    )
    r %>% mutate(
        nebd_frame = as.integer(nebd_frame),
        cell_line = str_replace_all(cell_line, " ", "_"),
        condition = str_replace_all(condition, " ", "_")
      ) %>% 
      unite(cellcon, c(cell_line, condition), sep="-", remove=FALSE) %>% 
      unite(mcell, c(movie, cell), sep="-", remove=FALSE) %>% 
      unite(cell_id, c(cellcon, mcell), remove=FALSE, sep=":") %>% 
      mutate(
        cell_file = file.path(fpath, glue("{name}.xls")),
        background_file = file.path(fpath, glue("{name}_extendedvol.xls")),
        info_file = fn
      ) %>% 
      mutate(date = as.Date(date))
  }) %>% 
    mutate_at(vars(name, cell_line, condition, cellcon, movie, mcell), as_factor)
    
  trcol <- map_dfr(meta$info_file, ~readxl::read_excel(.x, sheet="trackid")) %>%
    set_names(c("name", "track_id", "dot_colour")) %>%
    mutate(track_id = as.character(as.integer(track_id))) %>% 
    left_join(select(meta, name, cell_id), by="name")
  
  # check for duplicates
  dm <- meta %>% add_count(cell_id) %>% filter(n > 1)
  if(nrow(dm) > 0) {
    print(dm)
    stop("Duplicates detected")
  }
  tm <- trcol %>% add_count(cell_id, track_id) %>% filter(n > 1)
  if(nrow(tm) > 0) {
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
#' @export
read_excel_cell <- function(excel_file, sheets=NULL, verbose=TRUE) {
  if(!file.exists(excel_file)) return(NULL)
  if(verbose) cat(glue("\nReading {excel_file}\n\n"))
  if(is.null(sheets)){
    sheets <- excel_sheets(path.expand(excel_file))  # there is a bug in readxl that doesn't take relative paths
  }
  map(sheets, ~readxl::read_excel(path.expand(excel_file), .x, skip=1)) %>% set_names(sheets)
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
#' @param EXTVOL_SHEETS A vector with sheet names for background (extendedvol) files.
#'
#' @return A list with metadata, cells and track_colours.
#' @export
read_cells <- function(info, sheets, EXTVOL_SHEETS) {
  test_for_files(info$metadata)
  dots <- map(info$metadata$cell_file, ~read_excel_cell(.x, sheets)) %>%
    set_names(info$metadata$cell_id)
  extvol <- map(info$metadata$background_file, ~read_excel_cell(.x, EXTVOL_SHEETS)) %>%
    set_names(info$metadata$cell_id)
  list(
    metadata = info$metadata,
    track_colours = info$track_colours,
    dots = dots,
    extvol = extvol
  )
}


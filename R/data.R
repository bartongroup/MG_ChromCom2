# Extract cell names from files,
# based on trackid files
get_metadata <- function(path) {
  names <- dir(path, pattern="*trackid.xlsx") %>% 
    str_remove("_trackid.xlsx")
  tibble(
    name = as_factor(names)
  ) %>% 
    mutate(
      cell = name %>% str_extract("cell\\s\\d+") %>% str_remove("cell\\s") %>% as.integer(),
      condition = name %>% str_extract("^\\w+_") %>% str_remove("_") %>% as_factor(),
      cell_file = file.path(path, glue("{name}.xlsx")),
      cache_file = file.path("cache", glue("{name}.rds")),
      trackid_file = file.path(path, glue("{name}_trackid.xlsx")),
      nebd_file = file.path(path, glue("{name}_frameNEBD.xlsx"))
    )
}

# Read main Excel sheet with cell data
# Warning: original "xls" file does not work,
# needs to be converted into "xlsx"
# Returns named list of sheets
read_excel_cell <- function(excel_file, cache_file) {
  stopifnot(file.exists(excel_file))
  if(file.exists(cache_file)) {
    cat(glue("\nReading {cache_file}\n\n"))
    x <- read_rds(cache_file)
  } else {
    cat(glue("\nReading {excel_file}\n\n"))
    sheets <- excel_sheets(excel_file)
    x <- map(sheets, ~read_excel(excel_file, .x, skip=1)) %>% set_names(sheets)
    write_rds(x, cache_file)
  }
  return(x)
}

read_cells <- function(meta) {
  cls <- map2(meta$cell_file, meta$cache_file, ~read_excel_cell(.x, .y)) %>% set_names(meta$name)
  trids <- map_dfr(meta$trackid_file, ~read_excel(.x)) %>% set_names(c("name", "track_id", "colour")) %>%
    mutate(track_id = as.character(as.integer(track_id)))
  nebds <- map_dfr(meta$nebd_file, ~read_excel(.x)) %>% set_names(c("name", "nebd_frame"))
  list(
    metadata = meta,
    cells = cls,
    track_ids = trids,
    nebd_frames = nebds
  )
}


identify_colours <- function(inten) {
  tracks <- unique(inten$track_id)
  track_colours %>% 
    filter(track_id %in% tracks)
}

# Process raw data from one cell.
# r is raw excell data (by sheets)
# int.sel is intensity selection (e.g. "Median")
# track_ids = track_id data
# nebd_frames = NEBD frame data
process_raw_data <- function(r, cell_name, int.sel, track_ids, nebd_frames, meta) {
 
  ch1 <- r[[glue("Intensity {int.sel} Ch=1 Img=1")]] %>% 
    select(
      intensity_red = glue("Intensity {int.sel}"),
      track_id = TrackID,
      id = ID
    ) %>% 
    mutate(track_id = as.character(as.integer(track_id))) 
  
  ch2 <- r[[glue("Intensity {int.sel} Ch=2 Img=1")]] %>% 
    select(
      intensity_green = glue("Intensity {int.sel}"),
      id = ID
    ) 
  
  intensities <- full_join(ch1, ch2, by="id") %>% select(id, track_id, intensity_red, intensity_green)
  
  track_colour <- track_ids %>% filter(name == cell_name) %>% select(track_id, colour)
  
  times <- r$Time %>%
    set_names("time", "unit", "cat", "frame", "track_id", "id") %>% 
    mutate(time = time / 60, unit="min", track_id = as.character(as.integer(track_id)))
  
  nebd_frame <- nebd_frames %>% 
    filter(name == cell_name) %>% 
    pull(nebd_frame)
  nebd_time <- times %>% 
    filter(frame == nebd_frame) %>% 
    select(time) %>% 
    distinct() %>% 
    pull(time)
  
  this_meta <- meta %>% filter(name == cell_name)

  times <- times %>% 
    mutate(time_nebd = as.integer(round(time - nebd_time)))
  
  
  pos <-r$Position %>% 
    select(
      x = "Position X",
      y = "Position Y",
      z = "Position Z",
      frame = Time,
      track_id = TrackID,
      id = ID
    ) %>% 
    mutate(track_id = as.character(as.integer(track_id)))
  
  dat <- pos %>% 
    group_by(frame) %>% 
    mutate(n_dot = n()) %>% 
    group_by(frame, track_id) %>% 
    mutate(n_colour = n()) %>% 
    ungroup() %>% 
    mutate(cell = r$cell_id) %>% 
    left_join(track_colour, by="track_id") %>% 
    left_join(select(times, time, time_nebd, id), by="id")
  
  list(
    dat = dat,
    pos = pos,
    times = times,
    nebd_frame = nebd_frame,
    track_colour = track_colour,
    intensities = intensities,
    cell = this_meta$cell,
    condition = this_meta$condition
  )
}

process_all_raw_data <- function(raw, int.sel) {
  names <- raw$metadata$name
  map(names, ~process_raw_data(raw$cells[[.x]], .x, int.sel, raw$track_ids, raw$nebd_frames, raw$metadata)) %>% 
    set_names(names)
}


merge_cell_data <- function(d) {
  map_dfr(names(d), function(nm) {
    d[[nm]]$dat %>% 
      mutate(name = nm, cell = d[[nm]]$cell, condition = d[[nm]]$condition)
  })
}


process_parse_raw_data <- function(raw, int.sel = "Median") {
  celldat <- process_all_raw_data(raw, int.sel) 
  xyz <- merge_cell_data(celldat)
  parsed <- parse_states(xyz)
  list(
    metadata = raw$metadata,
    celldat = celldat,
    xyz = xyz,
    parsed = parsed
  )
}
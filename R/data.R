# Extract cell names from files,
# based on info files
get_metadata <- function(path) {
  files <- dir(path, pattern="*info.xlsx", full.names=FALSE)
  map_dfr(files, function(f) {
    d <- read_excel(file.path(path, f))
  }) %>% 
    set_names(c("name", "nebd_frame", "condition", "cell")) %>% 
    unite("cell_id", c(condition, cell), remove=FALSE, sep="_") %>% 
    mutate(
      cell_file = file.path(path, glue("{name}.xlsx")),
      cache_file = file.path("cache", glue("{name}.rds")),
      trackid_file = file.path(path, glue("{name}_trackid.xlsx")),
      info_file = file.path(path, glue("{name}_info.xlsx"))
    ) %>% 
    mutate_at(vars(name, condition), as_factor)
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


check_for_files <- function(meta) {
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

read_cells <- function(meta) {
  check_for_files(meta)
  cls <- map2(meta$cell_file, meta$cache_file, ~read_excel_cell(.x, .y)) %>% set_names(meta$cell_id)
  trids <- map_dfr(meta$trackid_file, ~read_excel(.x)) %>% set_names(c("name", "track_id", "colour")) %>%
    mutate(track_id = as.character(as.integer(track_id))) %>% 
    left_join(select(meta, name, cell_id), by="name")
  list(
    metadata = meta,
    cells = cls,
    track_ids = trids
  )
}


identify_colours <- function(inten) {
  tracks <- unique(inten$track_id)
  track_colours %>% 
    filter(track_id %in% tracks)
}

# Process raw data from one cell.
# r is raw excel data from one cell file
# int.sel is intensity selection (e.g. "Median")
# track_ids = track_id data
process_raw_data <- function(r, cell.id, int.sel, track_ids, meta) {
 
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
  
  track_colour <- track_ids %>% filter(cell_id == cell.id) %>% select(track_id, colour)
  
  times <- r$Time %>%
    set_names("time", "unit", "cat", "frame", "track_id", "id") %>% 
    mutate(time = time / 60, unit="min", track_id = as.character(as.integer(track_id)))
  
  this_meta <- meta %>% filter(cell_id == cell.id)
  
  nebd_frame <- this_meta %>% pull(nebd_frame)
  
  nebd_time <- times %>% 
    filter(frame == nebd_frame) %>% 
    select(time) %>% 
    distinct() %>% 
    pull(time)
  
  times <- times %>% 
    mutate(time_nebd = as.integer(round(time - nebd_time)))
  
  
  pos <-r$Position %>% 
    select(
      frame = Time,
      x = "Position X",
      y = "Position Y",
      z = "Position Z",
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
    condition = this_meta$condition,
    cell_id = cell.id
  )
}

process_all_raw_data <- function(raw, int.sel) {
  cells <- raw$metadata$cell_id
  map(cells, ~process_raw_data(raw$cells[[.x]], .x, int.sel, raw$track_ids, raw$metadata)) %>% 
    set_names(cells)
}


merge_cell_data <- function(d) {
  map_dfr(names(d), function(cellid) {
    d[[cellid]]$dat %>% 
      mutate(cell_id = cellid, .before=1)
  })
}


process_parse_raw_data <- function(raw, int.sel = "Median", dist.lightblue = 0, dist.brown = 0.75, dist.pink = 0.4) {
  md <- raw$metadata %>% select(cell_id, condition, cell)
  celldat <- process_all_raw_data(raw, int.sel) 
  xyz <- merge_cell_data(celldat) %>% left_join(md, by="cell_id")
  parsed <- parse_states(xyz, dist.lightblue, dist.brown, dist.pink) %>%
    left_join(md, by="cell_id")
  list(
    metadata = raw$metadata,
    celldat = celldat,
    xyz = xyz,
    parsed = parsed,
    params = list(
      dist_lightblue = dist.lightblue,
      dist_brown = dist.brown,
      dist_pink = dist.pink
    )
  )
}
read_raw_excel <- function(file) {
  stopifnot(file.exists(file))
  cat(glue("\nReading {file}\n\n"))
  sheets <- excel_sheets(file)
  x <- map(sheets, ~read_excel(file, .x, skip=1)) %>% set_names(sheets)
  return(x)
}

read_raw_files <- function(files) {
  cells <- files %>% str_extract("cell\\s\\d+") %>% str_replace("\\s", "_")
  r <- map(files, ~read_raw_excel(.x)) %>% set_names(cells)
  for(cl in cells) r[[cl]]$cell_id <- cl
  r
}

identify_colours_ <- function(inten) {
  inten %>% 
    mutate(colour = if_else(intensity_red >= intensity_green, "red", "green")) %>% 
    group_by(track_id) %>% 
    summarise(n_red = length(which(colour == "red")), n_green = length(which(colour == "green"))) %>% 
    mutate(colour = if_else(n_red > n_green, "red", "green")) %>% 
    select(track_id, colour)
}

identify_colours <- function(inten) {
  tracks <- unique(inten$track_id)
  track_colours %>% 
    filter(track_id %in% tracks)
}

process_raw_data <- function(r, int.sel) {
 
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
  
  track_colour <- track_colours %>% filter(cell == r$cell_id) %>% select(track_id, colour)
  
  times <- r$Time %>%
    set_names("time", "unit", "cat", "frame", "track_id", "id") %>% 
    mutate(time = time / 60, unit="min", track_id = as.character(as.integer(track_id)))
  
  nedb_frame <- nedb_frames %>% 
    filter(cell == r$cell_id) %>% 
    pull(nedb_frame)
  nedb_time <- times %>% 
    filter(frame == nedb_frame) %>% 
    select(time) %>% 
    distinct() %>% 
    pull(time)
  
  times <- times %>% 
    mutate(time_nedb = as.integer(round(time - nedb_time)))
  
  
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
    left_join(select(times, time, time_nedb, id), by="id")
  
  list(
    dat = dat,
    pos = pos,
    times = times,
    nedb_frame = nedb_frame,
    track_colour = track_colour,
    intensities = intensities
  )
}

process_all_raw_data <- function(raw, int.sel) {
  map(raw, ~process_raw_data(.x, int.sel))
}


merge_cell_data <- function(d) {
  map_dfr(names(d), function(cl) {
    d[[cl]]$dat %>% 
      mutate(cell = cl)
  })
}


process_parse_raw_data <- function(raw_dat, int.sel = "Median") {
  raw <- process_all_raw_data(raw_dat, int.sel) 
  xyz <- merge_cell_data(raw)
  parsed <- parse_states(xyz)
  list(
    raw = raw,
    xyz = xyz,
    parsed = parsed
  )
}
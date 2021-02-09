read_raw_excel <- function(file) {
  stopifnot(file.exists(file))
  sheets <- excel_sheets(file)
  map(sheets, ~read_excel(file, .x, skip=1)) %>% set_names(sheets)
}

read_raw_files <- function(files) {
  cells <- files %>% str_extract("cell\\s\\d+") %>% str_replace("\\s", "_")
  map(files, ~read_raw_excel(.x)) %>% set_names(cells)
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

process_raw_data <- function(r) {
 
  ch1 <- r$`Intensity Median Ch=1 Img=1` %>% 
    select(
      intensity_red = "Intensity Median",
      track_id = TrackID,
      id = ID
    ) %>% 
    mutate(track_id = as.character(as.integer(track_id))) 
  
  ch2 <- r$`Intensity Median Ch=2 Img=1` %>% 
    select(
      intensity_green = "Intensity Median",
      id = ID
    ) 
  
  intensities <- full_join(ch1, ch2, by="id") %>% select(id, track_id, intensity_red, intensity_green)
  
  track_colour <- identify_colours(intensities)
  
  times <- r$Time %>%
    set_names("time", "unit", "cat", "time_point", "track_id", "id")
  
  pos <-r$Position %>% 
    select(
      x = "Position X",
      y = "Position Y",
      z = "Position Z",
      time_point = Time,
      track_id = TrackID,
      id = ID
    ) %>% 
    mutate(track_id = as.character(as.integer(track_id)))
  
  dat <- pos %>% 
    group_by(time_point) %>% 
    mutate(n_dot = n()) %>%
    group_by(time_point, track_id) %>% 
    mutate(dot_id = row_number()) %>% 
    ungroup() %>% 
    
    left_join(track_colour, by="track_id") %>% 
    left_join(select(times, time, id), by="id")

  list(
    dat = dat,
    pos = pos,
    times = times,
    track_colour = track_colour,
    intensities = intensities
  )
}

process_all_raw_data <- function(raw) {
  map(raw, ~process_raw_data(.x))
}


merge_cell_data <- function(d) {
  map_dfr(names(d), function(cl) {
    d[[cl]]$dat %>% 
      mutate(cell = cl)
  })
}
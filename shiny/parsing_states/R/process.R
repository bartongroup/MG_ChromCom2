#' Process raw time data
#'
#' @param r Raw data, one cell from the object created by read_cells.
#' @param cellid Full cell id, e.g. "TT206_3"
#' @param meta Metadata tibble.
#'
#' @return A list with 'nebd_frame' (integer) and a tibble 'times', containing
#'   time information.
#' @export
process_times <- function(r, cellid, meta) {
  times <- r$Time %>%
    set_names("time", "unit", "cat", "frame", "track_id", "id") %>% 
    mutate(time = time / 60, unit="min", track_id = as.character(as.integer(track_id)), frame = as.integer(frame))
  
  nebd_frame <- meta %>% filter(cell_id == cellid) %>% pull(nebd_frame)
  
  nebd_time <- times %>% 
    filter(frame == nebd_frame) %>% 
    select(time) %>% 
    distinct() %>% 
    pull(time)
  
  times <- times %>% 
    mutate(time_nebd = as.integer(round(time - nebd_time))) %>% 
    arrange(time_nebd)
  
  list(
    nebd_frame = nebd_frame,
    times = times
  )
}


#' Process volume table from raw data
#'
#' @param r Raw data, one cell from the object created by read_cells.
#' @param volname Name of the new volume column
#'
#' @return Tibble with ID, volume and, if found, track_id
#' @export
#'
#' @examples
process_volume <- function(r, volname) {
  if(is.null(r$Volume[["TrackID"]])) {
    r$Volume %>% 
      select(ID, Volume) %>% 
      set_names(c("id", volname))
  } else {
    r$Volume %>% 
      mutate(track_id = as.character(as.integer(TrackID))) %>% 
      select(ID, track_id, Volume) %>% 
      set_names(c("id", "track_id", volname))
  }
}

#' Process raw channel colour data for dots
#'
#' @param r Raw data, one cell from the object created by read_cells.
#' @param stats Vector of statistics for channel intensities, e.g. "Mean" or "Median"
#' @param intensity_prefix How to prefix column with combined intensities.
#' @param track_colour Table with colour per track
#'
#' @return
#' @export
#'
#' @examples
process_dots_intensities <- function(r, stats, intensity_prefix, track_colour) {
  map_dfr(stats, function(stat) {
    ch1 <- r[[glue("Intensity {stat} Ch=1 Img=1")]] %>% 
      select(
        red = glue("Intensity {stat}"),
        track_id = TrackID,
        id = ID
      ) %>% 
      mutate(track_id = as.character(as.integer(track_id))) 
    
    ch2 <- r[[glue("Intensity {stat} Ch=2 Img=1")]] %>% 
      select(
        green = glue("Intensity {stat}"),
        id = ID
      ) 
    
    full_join(ch1, ch2, by="id") %>%
      select(id, track_id, red, green) %>% 
      pivot_longer(-c(id, track_id), names_to="chn_colour", values_to="intensity") %>% 
      add_column(what = paste(intensity_prefix, tolower(stat), sep="_"))
  }) %>% 
    pivot_wider(id_cols=c(id, track_id, chn_colour), values_from=intensity, names_from=what) %>% 
    left_join(track_colour, by="track_id") %>% 
    relocate(dot_colour, .after="track_id")
}


#' Process raw channel colour data for extended volume
#'
#' @param r Raw data, one cell from the object created by read_cells.
#' @param stats Vector of statistics for channel intensities, e.g. "Mean" or "Median"
#' @param intensity_prefix How to prefix column with combined intensities.
#' @param track_colour Table with colour per track
#'
#' @return
#' @export
#'
#' @examples
process_extvol_intensities <- function(r, stats, intensity_prefix, track_colour) {
  map_dfr(stats, function(stat) {
    ch1 <- r[[glue("Intensity {stat} Ch=1 Img=1")]] %>% 
      select(
        red = glue("Intensity {stat}"),
        time = Time,
        id = ID
      ) 
    
    ch2 <- r[[glue("Intensity {stat} Ch=2 Img=1")]] %>% 
      select(
        green = glue("Intensity {stat}"),
        id = ID
      )
    
    full_join(ch1, ch2, by="id") %>%
      select(id, red, green) %>% 
      pivot_longer(-id, names_to="chn_colour", values_to="intensity") %>% 
      add_column(what = paste(intensity_prefix, tolower(stat), sep="_"))
  }) %>% 
    distinct() %>% 
    pivot_wider(id_cols=c(id, chn_colour), values_from=intensity, names_from=what)
}


#' Process raw data from one cell
#'
#' Converts raw data read by read_cells into more usable format. Also, assigns
#' colours and calculates time since NEBD.
#'
#' @param r Raw data, one cell from the object created by read_cells.
#' @param cellid Full cell id, e.g. "TT206_3"
#' @param track_colours Tibble with track colours, part of the object created by read_cells.
#' @param meta Metadata tibble.
#' @param stats Vector of statistics for channel intensities, e.g. "Mean" or "Median"
#'
#' @return A list with lots of goodies.
#' @export
process_dots_raw_data <- function(r, cellid, track_colours, meta, stats) {
  
  track_colour <- track_colours %>%
    filter(cell_id == cellid) %>%
    select(track_id, dot_colour)
  
  volumes <- process_volume(r, "dots_volume")
  time_track <- process_times(r, cellid, meta)
  intensities <- process_dots_intensities(r, stats, "dots", track_colour) %>% 
    left_join(time_track$times %>%  select(id, frame, time_nebd), by="id") %>% 
    left_join(volumes, by=c("id", "track_id"))
  
  pos <- r$Position %>% 
    select(
      frame = Time,
      x = "Position X",
      y = "Position Y",
      z = "Position Z",
      track_id = TrackID,
      id = ID
    ) %>% 
    mutate(track_id = as.character(as.integer(track_id)), frame = as.integer(frame))
  
  dat <- pos %>% 
    group_by(frame) %>% 
    mutate(n_dot = n()) %>% 
    group_by(frame, track_id) %>% 
    mutate(n_colour = n()) %>% 
    ungroup() %>% 
    left_join(select(time_track$times, time, time_nebd, id), by="id") %>% 
    left_join(track_colour, by="track_id")

  this_meta <- meta %>% filter(cell_id == cellid) 
  
  list(
    dat = dat,
    pos = pos,
    times = time_track$times,
    nebd_frame = time_track$nebd_frame,
    track_colour = track_colour,
    intensities = intensities,
    cell = this_meta$cell,
    cell_line = this_meta$cell_line,
    condition = this_meta$condition,
    cell_id = cellid
  )
}

#' Process data from all cells (internal function)
#'
#' @param rw Raw data object crated by read_cells
#'
#' @return A named list, one element per cell.
#' @export
process_all_dots_raw_data <- function(rw, stats) {
  cells <- rw$metadata$cell_id
  map(cells, ~process_dots_raw_data(rw$dots[[.x]], .x, rw$track_colours, rw$metadata, stats)) %>% 
    set_names(cells)
}


#' Process background data from one cell
#'
#' Converts background data read by read_cells into table of mean intensities per time point.
#'
#' @param r Raw data, one cell from the object created by read_cells.
#' @param cellid Full cell id, e.g. "TT206_3"
#' @param stat Statistic to be used (Mean, Median)
#'
#' @return A list with lots of goodies.
#' @export
process_extvol_raw_data <- function(r, cellid, meta, stats) {
  
  if(is.null(r)) {
    extvol <- tibble(
      frame = numeric(),
      chn_colour = character(),
      background = numeric(),
      time = numeric()
    )
  } else {
    
    times <- r$Time %>%
      set_names("time", "unit", "cat", "frame", "id") %>% 
      mutate(time = time / 60, unit="min", frame = as.integer(frame))
    
    frame_time <- times %>% 
      select(frame, time) %>% 
      distinct()
    
    volumes <- process_volume(r, "extvol_volume")
    
    extvol <- process_extvol_intensities(r, stats, "extvol", track_colour) %>% 
      left_join(select(times, id, frame), by="id") %>% 
      left_join(volumes, by="id") %>% 
      pivot_longer(-c(id, frame, chn_colour)) %>% 
      group_by(frame, chn_colour, name) %>% 
      summarise(value = mean(value)) %>% 
      pivot_wider(id_cols = c(frame, chn_colour)) %>% 
      left_join(frame_time, by="frame")
  }
    
  list(
    extvol = extvol,
    cell_id = cellid
  )
}


#' Process data from all backgrounds (internal function)
#'
#' @param rw Raw data object crated by read_cells
#'
#' @return A named list, one element per cell.
#' @export
process_all_extvol_raw_data <- function(rw, stats) {
  cells <- rw$metadata$cell_id
  map(cells, ~process_extvol_raw_data(rw$extvol[[.x]], .x, rw$metadata, stats)) %>% 
    set_names(cells)
}


#' Merge data from all cells (internal function)
#'
#' @param d Celldat object.
#'
#' @return Tibble
#' @export
merge_cell_data <- function(d, what="dat") {
  map_dfr(names(d), function(cellid) {
    d[[cellid]][[what]] %>% 
      mutate(cell_id = cellid, .before=1)
  })
}


#' Process raw data
#'
#' The main function to prepare data for downstream analysis. I takes a raw data
#' object (created by read_cells) and creates a list of coordinates and metadata.
#'
#' @param rw Object with raw data.
#' @param z_correction Correction to z coordinates due to different refraction in oil-based objective
# and water-based medium with cells.
#' @param with_celldat Logical if the output object should include input raw data.
#'
#' @return List of metadata, xyz, parsed, params and (optional) celldat.
#' Only xyz contains corrected z-coordinate, celldat contains original raw coordinates.
#' @export
#'
#' @examples
#' metadata = get_metadata("data")
#' raw = read_cells(metadata, CELL_SHEETS)
#' dat = process_raw_data(raw) %>% parse_xyz_data(params)
#' 
process_raw_data <- function(rw, z_correction = 0.85,
                             dots_stats=c("Sum", "Max", "Mean"),
                             extvol_stats=c("Sum", "Mean", "Min")) {
  md <- rw$metadata %>% select(cell_id, cell_line, condition, movie, cell, cellcon, mcell)
  
  dotsdat <- process_all_dots_raw_data(rw, dots_stats)
  xyz <- merge_cell_data(dotsdat) %>%
    left_join(md, by="cell_id") %>%
    mutate(
      z = z * z_correction,
      dot_colour = factor(dot_colour, levels=c("green", "red"))
    )
  ints_dots <- merge_cell_data(dotsdat, "intensities") %>% 
    select(-c(id, track_id)) %>% 
    distinct()
  
  ints_extvol <- process_all_extvol_raw_data(rw, extvol_stats) %>%
    merge_cell_data("extvol")
  
  intensities <- ints_dots %>% 
    left_join(ints_extvol, by=c("cell_id", "frame", "chn_colour")) %>% 
    select(cell_id, frame, time, time_nebd, dot_colour, chn_colour, starts_with("dots"), starts_with("extvol"))
  
  list(
    metadata = md,
    xyz = xyz,
    intensities = intensities,
    dotsdat = dotsdat
  )
}


#' Parse processed xyz data
#'
#' @param d Processed data created by `process_raw_data`
#' @param params List with parsing parameters.
#'
#' @return Original object with "parsed" tibble added.
#' @export
#'
#' @examples
#' metadata = get_metadata("data")
#' raw = read_cells(metadata, CELL_SHEETS)
#' dat = process_raw_data(raw) %>% parse_xyz_data(params)
#' 
parse_xyz_data <- function(d, params) {
  md <- d$metadata %>%
    select(cell_id, cell_line, condition, movie, cell, cellcon, mcell)
  parsed <- parse_states(d$xyz, params) %>%
    left_join(md, by="cell_id")
  d$parsed <- parsed
  d$params <- params
  d
}

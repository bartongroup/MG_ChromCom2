
#' Process raw data from one cell
#'
#' Converts raw data read by read_cells into more usable format. Also, assigns
#' colours and calculates time since NEBD.
#'
#' @param r Raw data, one cell from the object created by read_cells.
#' @param cellid Full cell id, e.g. "TT206_3"
#' @param track_colours Tibble with track colours, part of the object created by read_cells.
#' @param meta Metadata tibble.
#'
#' @return A list with lots of goodies.
#' @export
process_cell_raw_data <- function(r, cellid, track_colours, meta, stat) {
  
  ch1 <- r[[glue("Intensity {stat} Ch=1 Img=1")]] %>% 
    select(
      intensity_red = glue("Intensity {stat}"),
      track_id = TrackID,
      id = ID
    ) %>% 
    mutate(track_id = as.character(as.integer(track_id))) 
  
  ch2 <- r[[glue("Intensity {stat} Ch=2 Img=1")]] %>% 
    select(
      intensity_green = glue("Intensity {stat}"),
      id = ID
    ) 
  
  intensities <- full_join(ch1, ch2, by="id") %>%
    select(id, track_id, intensity_red, intensity_green) %>% 
    pivot_longer(-c(id, track_id), names_to="colour", values_to="intensity", names_prefix = "intensity_")
  
  track_colour <- track_colours %>%
    filter(cell_id == cellid) %>%
    select(track_id, colour)
  
  times <- r$Time %>%
    set_names("time", "unit", "cat", "frame", "track_id", "id") %>% 
    mutate(time = time / 60, unit="min", track_id = as.character(as.integer(track_id)), frame = as.integer(frame))
  
  this_meta <- meta %>% filter(cell_id == cellid)
  
  nebd_frame <- this_meta %>% pull(nebd_frame)
  
  nebd_time <- times %>% 
    filter(frame == nebd_frame) %>% 
    select(time) %>% 
    distinct() %>% 
    pull(time)
  
  times <- times %>% 
    mutate(time_nebd = as.integer(round(time - nebd_time)))
  
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
    left_join(track_colour, by="track_id") %>% 
    left_join(select(times, time, time_nebd, id), by="id") %>% 
    left_join(intensities, by=c("id", "track_id", "colour"))
  
  list(
    dat = dat,
    pos = pos,
    times = times,
    nebd_frame = nebd_frame,
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
#' @param raw Raw data object crated by read_cells
#'
#' @return A named list, one element per cell.
#' @export
process_cells_raw_data <- function(raw, stat) {
  cells <- raw$metadata$cell_id
  map(cells, ~process_cell_raw_data(raw$cells[[.x]], .x, raw$track_colours, raw$metadata, stat)) %>% 
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
process_cell_background_data <- function(r, cellid, stat) {
  
  if(is.null(r)) {
    bkg <- tibble(
      frame = numeric(),
      background_red = numeric(),
      background_green = numeric(),
      time = numeric()
    )
  } else {
    
    ch1 <- r[[glue("Intensity {stat} Ch=1 Img=1")]] %>% 
      select(
        background_red = glue("Intensity {stat}"),
        time = Time,
        id = ID
      ) 
    
    ch2 <- r[[glue("Intensity {stat} Ch=2 Img=1")]] %>% 
      select(
        background_green = glue("Intensity {stat}"),
        id = ID
      )
    
    times <- r$Time %>%
      set_names("time", "unit", "cat", "frame", "id") %>% 
      mutate(time = time / 60, unit="min", frame = as.integer(frame))
    
    frame_time <- times %>% 
      select(frame, time) %>% 
      distinct()
    
    bkg <- full_join(ch1, ch2, by="id") %>%
      select(id, background_red, background_green) %>% 
      pivot_longer(-id, names_to="colour", values_to="background", names_prefix = "background_") %>% 
      left_join(select(times, id, frame), by="id") %>% 
      group_by(frame, colour) %>% 
      summarise(background = mean(background)) %>% 
      left_join(frame_time, by="frame")
  }
    
  list(
    dat = bkg,
    cell_id = cellid
  )
}


#' Process data from all backgrounds (internal function)
#'
#' @param raw Raw data object crated by read_cells
#'
#' @return A named list, one element per cell.
#' @export
process_cells_background_data <- function(raw, stat="Mean") {
  cells <- raw$metadata$cell_id
  map(cells, ~process_cell_background_data(raw$background[[.x]], .x, stat)) %>% 
    set_names(cells)
}


#' Merge data from all cells (internal function)
#'
#' @param d Celldat object.
#'
#' @return Tibble
#' @export
merge_cell_data <- function(d) {
  map_dfr(names(d), function(cellid) {
    d[[cellid]]$dat %>% 
      mutate(cell_id = cellid, .before=1)
  })
}


#' Process raw data
#'
#' The main function to prepare data for downstream analysis. I takes a raw data
#' object (created by read_cells) and creates a list of coordinates and metadata.
#'
#' @param raw Object with raw data.
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
#' raw = read_cells(metadata, cell_sheets)
#' dat = process_raw_data(raw) %>% parse_xyz_data(params)
#' 
process_raw_data <- function(raw, z_correction = 0.85, with_celldat=TRUE, cell_stat="Max", bkg_stat="Mean") {
  md <- raw$metadata %>% select(cell_id, cell_line, condition, movie, cell, cellcon, mcell)
  celldat <- process_cells_raw_data(raw, cell_stat)
  bkg <- process_cells_background_data(raw, bkg_stat) %>% merge_cell_data()
  xyz <- merge_cell_data(celldat) %>%
    left_join(md, by="cell_id") %>%
    mutate(
      z = z * z_correction,
      colour = factor(colour, levels=c("green", "red"))
    ) %>% 
    left_join(select(bkg, -time), by=c("cell_id", "frame", "colour"))
  r <- list(
    metadata = md,
    xyz = xyz
  )
  if(with_celldat) {
    r$celldat <- celldat
    r$bkg <- bkg
  }
  r
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
#' raw = read_cells(metadata, cell_sheets)
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
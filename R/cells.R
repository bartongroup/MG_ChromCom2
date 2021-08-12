
# convert new data to ChrCom3 object
convert_to_chrcom3 <- function(dat) {
  pars <- c3pars()
  tdat <- dat$parsed %>% 
    mutate(
      letter = as.character(letter)
    ) %>% 
    arrange(time_nebd) %>% 
    pivot_wider(id_cols=cell, names_from=time_nebd, values_from=letter) %>% 
    select(-cell) %>% 
    as.matrix()
  tim <- colnames(tdat) %>% as.numeric()
  
  echr <- ChromCom3(pars, time=tim, cells=tdat, colours=unique(dat$parsed$letter))
}


get_timepoint_raw_data <- function(rw, xyz, cellid, tim) {
  ids <- xyz %>% 
    filter(cell_id == cellid & time_nebd == tim) %>% 
    pull(id)
  sheets <- c("Time", "Position", "Intensity Max Ch=1 Img=1", "Intensity Max Ch=2 Img=1")
  map(sheets, function(sheet) {
    rw$cells[[cellid]][[sheet]] %>% filter(ID %in% ids) %>% mutate(TrackID = as.character(as.integer(TrackID)))
  }) %>% 
    set_names(sheets)
}
plan_data <- function() {
  
  get_data <- drake_plan(
    params = list(dist.lightblue = 0.4, dist.brown = 0.75, dist.pink = 0.4, black.length = 5),
    metadata = get_metadata("data"),
    raw = read_cells(metadata, cell_sheets),
    dat = process_raw_data(raw) %>% parse_xyz_data(params)
  )
  
  bind_rows(
    get_data
  )
}
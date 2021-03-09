plan_data <- function() {
  
  get_data <- drake_plan(
    params = list(dist.lightblue = 0.4, dist.brown = 0.75, dist.pink = 0.4, black.length = 5),
    metadata = get_metadata("data"),
    raw = read_cells(metadata),
    dat = process_parse_raw_data(raw, params) 
  )
  
  bind_rows(
    get_data
  )
}
plan_data <- function() {
  
  get_data <- drake_plan(
    metadata = get_metadata("data"),
    raw = read_cells(metadata),
    dat = process_parse_raw_data(raw, int.sel = "Median", dist.lightblue = 0, dist.brown = 0.75, dist.pink = 0.4) 
  )
  
  bind_rows(
    get_data
  )
}
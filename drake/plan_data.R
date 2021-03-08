plan_data <- function() {
  
  get_data <- drake_plan(
    metadata = get_metadata("data"),
    raw = read_cells(metadata),
    dat = process_parse_raw_data(raw, int.sel = "Median") 
  )
  
  bind_rows(
    get_data
  )
}
plan_data <- function() {
  
  get_data <- drake_plan(
    raw = read_raw_files(input_files),
    dat = process_parse_raw_data(raw) 
  )
  
  bind_rows(
    get_data
  )
}
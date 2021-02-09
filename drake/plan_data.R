plan_data <- function() {
  
  get_data <- drake_plan(
    raw = read_raw_files(input_files)
  )
  
  process_data <- drake_plan(
    dat_cells = process_all_raw_data(raw),
    dat = merge_cell_data(dat_cells)
  )
  
  bind_rows(
    get_data,
    process_data
  )
}
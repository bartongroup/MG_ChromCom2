input_files <- c(
  "data/TT206_1NM-PP1_1_1_R3D_D3D_cell 1_fr1-68.xlsx",
  "data/TT206_1NM-PP1_1_1_R3D_D3D_cell 2_fr1-65.xlsx",
  "data/TT206_1NM-PP1_1_1_R3D_D3D_cell 3_fr1-94.xlsx"
)

track_colours <- tribble(
  ~cell, ~track_id, ~colour,
  
  "cell_1", "1000000000", "red",
  "cell_1", "1000000001", "green",
  "cell_1", "1000000099", "red",
  "cell_1", "1000000101", "green",
  
  "cell_2", "1000000000", "red",
  "cell_2", "1000000001", "green",
  "cell_2", "1000000069", "red",
  "cell_2", "1000000071", "green",
  
  "cell_3", "1000000000", "red",
  "cell_3", "1000000002", "green",
  "cell_3", "1000000154", "red",
  "cell_3", "1000000155", "green",
)
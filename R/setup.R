input_files <- c(
  "data/TT206_1NM-PP1_1_1_R3D_D3D_cell 1_fr1-68.xlsx",
  "data/TT206_1NM-PP1_1_1_R3D_D3D_cell 2_fr1-65.xlsx",
  "data/TT206_1NM-PP1_1_1_R3D_D3D_cell 3_fr1-94.xlsx"
)

nedb_frames <- tribble(
  ~cell, ~nedb_frame,
  "cell_1", 38,
  "cell_2", 40,
  "cell_3", 52
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


state_colour <- tribble(
  ~state, ~colour, ~letter,
  "none", "grey70", "N",
  "black", "black", "K",
  "lightblue", "skyblue", "L",
  "darkblue", "mediumblue", "B",
  "brown", "brown", "W",
  "pink", "lightpink", "P",
  "red", "red", "R"
) %>% 
  mutate(state = as_factor(state))

state_limit <- c(
  "lightblue" = 0.3,
  "brown" = 0.75,
  "pink"=  0.4
)

state_limit_tb <- tibble(
  state = names(state_limit),
  limit = state_limit
) %>% 
  mutate(state =factor(state, levels=state_colour$state))

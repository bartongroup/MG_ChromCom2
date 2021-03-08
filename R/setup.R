if(!dir.exists("cache")) dir.create("cache")

state_colour <- tribble(
  ~state, ~colour, ~letter,
  "none", "grey70", "-",
  "black", "black", "K",
  "lightblue", "skyblue", "L",
  "darkblue", "mediumblue", "B",
  "brown", "brown", "W",
  "pink", "lightpink", "P",
  "red", "red", "R"
) %>% 
  mutate(state = as_factor(state))

state_limit <- c(
  "lightblue" = 0.0,  # for now; black should take into account five consecutive states
  "brown" = 0.75,
  "pink"=  0.4
)

state_limit_tb <- tibble(
  state = names(state_limit),
  limit = state_limit
) %>% 
  mutate(state =factor(state, levels=state_colour$state))

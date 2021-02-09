plan_figures <- function() {
  
  anims <- drake_plan(
    anim_cell_1 = animate_cell(dat, "cell_1")
  )
  
  bind_rows(
    anims
  )
}
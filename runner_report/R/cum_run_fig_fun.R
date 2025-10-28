#' Graph cumulative distance per year
#' 

cum_run_fig_fun <- function(x = cum_runs){
  cum_run_fig <- ggplot(
    data = x, 
    aes(x = day_of_year, y = cum_dist, group = yr)) + 
    geom_step(aes(colour = line_clr)) + 
    geom_text_repel(
      aes(colour = line_clr, label = yr_lbl), 
      direction = "x", vjust = 0,
      segment.size = .7,
      segment.alpha = .5,
      segment.linetype = "dotted",
      box.padding = .4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20
      , xlim = 365.25
    ) + 
    scale_color_identity("", aes(colour = line_clr)) + 
    scale_y_continuous("Cumulative distance (km)") + 
    scale_x_continuous(
      "Day of the year"
      , breaks = c(29, 57,85, 113, 141, 169, 197, 225, 253, 281, 309, 337, 365)
      # , labels = c(29, 57,85, 113, 141, 169, 197, 225, 253, 281, 309, 337, 365) 
      , limits = c(0, 390)
      , expand = c(0,0)
    ) + 
    labs(title = glue("Cumulative distance run by year, {min(cum_runs$yr)} to {max(cum_runs$yr)}")) + 
    theme(legend.position = "none")
  return(cum_run_fig)
}
  
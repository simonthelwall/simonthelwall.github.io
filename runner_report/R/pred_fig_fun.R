predictor_fig_fun <- function(distance = "5k", predictor, pred_str = ""){
  if(distance == "5k"){
    p <- ggplot(
      data = filter(my_runs, distance_type == !!distance & elapsed_time < 3500),
      aes(x = {{predictor}}, y = elapsed_time)
    )
  }else{
    p <- ggplot(
      data = filter(my_runs, distance_type == !!distance),
      aes(x = {{predictor}}, y = elapsed_time)
    )
  }
  p <- p + geom_smooth(method = "lm", formula = y ~ splines::ns(x, df = 3)) + 
    geom_point() + 
    scale_y_continuous("Elapsed time (minutes)", labels = function(x) round(x/60, 1)) + 
    scale_x_continuous(name = pred_str)
  return(p)
}
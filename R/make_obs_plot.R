make_obs_plot <- function(df = all_obs, 
                          plot_var, 
                          plot_label) {
  
  plot_obs <- df |> 
    filter(variable == plot_var) |> 
    ggplot(aes(x = datetime)) +
    geom_point(aes(y = observation), size = 0.5) +
    scale_x_datetime(breaks = as_datetime(c("2023-07-01", "2024-01-01", "2024-07-01",
                                            "2025-01-01")),
                     date_labels = "%b %Y") +
    theme_bw() +
    labs(y=plot_label, x = 'Date') +
    coord_cartesian(clip = "off", 
                    xlim = c(as_datetime('2023-07-01'), as_datetime('2025-05-01')))
  return(plot_obs)
}


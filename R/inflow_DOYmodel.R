# simple forecast for inflow variables
inflows_DOY <- function(var, df, forecast_dates, reference_datetime) {
  
  forecast_dates_vector <- data.frame(datetime = forecast_dates) |> 
    mutate(doy = yday(datetime))
  
  out <- df  |> 
    filter(variable == var) |> 
    mutate(doy = lubridate::yday(datetime)) |> 
    group_by(variable, doy, site_id) |> 
    summarise(mu = mean(observation, na.rm = T),
              sigma = sd(observation, na.rm = T), .groups = 'drop') |> 
    right_join(forecast_dates_vector) |> 
    select(any_of(c('datetime', 'site_id', 'variable',  'mu', 'sigma'))) |> 
    group_by(datetime, site_id, variable) |> 
    # sample from the distribution based on the mean and sd, 31 ensemble members
    reframe(prediction = rnorm(31, mean = mu, sd = sigma)) |> 
    arrange(datetime, site_id, prediction) |> 
    group_by(site_id, datetime) |> 
    # parameter value needs to be character
    mutate(parameter = as.character(row_number()),
           # model_id = ensemble_name, 
           reference_datetime = start - days(1),
           variable = var,
           family = 'ensemble',
           flow_type = 'inflow', 
           flow_number = 1,
           datetime = as_datetime(datetime))
  
  return(out)
}

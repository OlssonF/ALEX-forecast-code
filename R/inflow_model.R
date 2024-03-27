# simple forecast for inflow variables
inflows_DOY <- function(var, df, forecast_dates, reference_datetime) {
  
  forecast_dates_vector <- data.frame(datetime = forecast_dates) |> 
    mutate(doy = yday(datetime))
  
  out <- df  |> 
    filter(variable == var) |> 
    mutate(doy = lubridate::yday(datetime)) |> 
    group_by(variable, doy, site_id) |> 
    summarise(prediction = mean(observation, na.rm = T), .groups = 'drop') |> 
    right_join(forecast_dates_vector) |> 
    select(any_of(c('datetime', 'site_id', 'variable',  'prediction'))) |> 
    mutate(model_id = 'DOY_inflow', 
           family = 'ensemble', 
           parameter = 1, 
           flow_type = 'inflow', 
           flow_number = 1,
           reference_datetime = as_datetime(start),
           datetime = as_datetime(datetime))
  
  return(out)
}

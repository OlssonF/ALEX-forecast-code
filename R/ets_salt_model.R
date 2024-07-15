ets_salt_model <- function(salt_targets,
                           horizon,
                           df_future, 
                           reference_datetime, 
                           inflow_model){
  
  # salt_targets <- inflow_targets |>
  #   dplyr::filter(variable == 'SALT') |> 
  #   rename(date = datetime) |> 
  #   mutate(observation = na.interp(observation))
  salt_targets_ts <- salt_targets |>
    filter(date < reference_datetime) |> 
    mutate(date = as_date(date)) |> 
    tsibble::as_tsibble(index = date) |> 
    tsibble::fill_gaps() |> 
    mutate(observation = zoo::na.approx(observation)) 
  
  salt_fit <- salt_targets_ts |> 
    model(ETS(observation))
  
  n_members <- max(df_future$ensemble)
  
  salt_predictions <- salt_fit |>
    generate(h = 30, times = n_members) |> 
    as_tibble() |> 
    mutate(.rep = as.numeric(.rep) -1) |> 
    rename(parameter = .rep,
           prediction = .sim) 
  
  #salt_build <- salt_predictions |> select(datetime)
  
  prediction_df <- salt_predictions |> 
    mutate(model_id = inflow_model) |>
    mutate(site_id = salt_targets$site_id[1]) |>
    mutate(reference_datetime = reference_datetime) |>
    mutate(family = 'ensemble') |>
    mutate(variable = 'SALT') |>
    mutate(flow_type = 'inflow') |>
    mutate(flow_number = 1) |>
    rename(datetime = date) |> 
    select(model_id, site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_type, flow_number)
  
  return(prediction_df)
  
}

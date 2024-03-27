
run_inflow_temperature_model <- function(met_df, met_past_df, met_combined, targets_df){ # met_past = df_past
  
  ## combine data with met drivers
  forecast_drivers <- met_df |> 
    left_join(targets_df, by = c('date')) |> 
    drop_na(water_temperature)
  
  
  ## CREATE MODEL AND FIT
  
  train_data <- forecast_drivers |> 
    filter(date < reference_datetime)
  
  ## define folds in training data 
  folds <- vfold_cv(train_data, v = 5) # orginally set to 10
  
  #set the recipe
  rec <- recipe(water_temperature ~ doy + temperature,
                data = train_data)
  
  rec_preprocess <- rec |> 
    step_normalize(all_numeric_predictors()) #|> 
  #step_dummy(doy)
  
  ## define model and tunining parameters (tuning 2/8 parameters right now)
  xgboost_mod <- boost_tree(tree_depth = tune(), trees = tune()) |> #, learn_rate = tune()) |> 
    set_mode("regression") |>  
    set_engine("xgboost")
  
  # define the model workflow
  xgboost_inflow_wkflow <- 
    workflow() %>% 
    add_model(xgboost_mod) %>% 
    add_recipe(rec_preprocess)
  
  # tune the hyper-parameters
  inflow_resample_fit <- xgboost_inflow_wkflow |> 
    tune_grid(resamples = folds, 
              grid = 25, 
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
  
  # show the results from tuning 
  inflow_resample_fit %>% 
    collect_metrics() |> 
    arrange(mean)
  
  # select the best tuned hyper-parameters
  best_hyperparameters <- inflow_resample_fit %>%
    select_best(metric = "rmse")
  
  final_workflow <- xgboost_inflow_wkflow |> 
    finalize_workflow(best_hyperparameters)
  
  ## fit the model (using all available data (past and future) for now but could just use training data)
  xgboost_inflow_fit <- fit(final_workflow, data = forecast_drivers)
  
  
  ## Use the foundation of all met events to create a prediction for every historical day 
  
  ## prepare met data to align with model
  create_historical_df <- met_combined |> 
    filter(variable == 'temperature_2m') |> 
    mutate(date = as.Date(datetime)) |> 
    summarise(temperature = mean(prediction, na.rm = TRUE), .by = c("date",'ensemble')) |> 
    mutate(doy = lubridate::yday(date)) |> 
    select(date, ensemble, doy, temperature)
  
  
  #make empty dataframe to store predictions
  data_build <- data.frame()
  
  for (i in unique(create_historical_df$ensemble)){
    
    ens_df <- create_historical_df |> 
      filter(ensemble == i)
    
    ens_inflow_temp <- predict(xgboost_inflow_fit, new_data = ens_df)
    
    ens_predictions <- cbind(ens_df,ens_inflow_temp) |> 
      rename(prediction = .pred) |> 
      mutate(prediction = ifelse(prediction < 0, 0, prediction))
    
    
    data_build <- bind_rows(data_build,ens_predictions)
    
  }
  
  
  ## join observations back onto predictions
  
  ## overwrite predictions with observed data when present
  update_historical_df <- data_build |> 
    left_join(targets_df, by = c('date')) |> 
    mutate(prediction = ifelse(!is.na(water_temperature), water_temperature, prediction)) |> 
    mutate(model_id = config$inflow$forecast_inflow_model) |> 
    mutate(site_id = config$location$site_id) |> 
    mutate(reference_datetime = config$run_config$forecast_start_datetime) |> 
    mutate(family = 'ensemble') |> 
    mutate(variable = 'TEMP') |> 
    mutate(flow_type = 'inflow') |> 
    mutate(flow_number = 1) |> 
    rename(parameter = ensemble, datetime = date) |> 
    select(model_id, site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_type, flow_number)
  
  return(update_historical_df)
}

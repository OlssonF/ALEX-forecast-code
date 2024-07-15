xg_run_inflow_model <- function(train_data, model_recipe, met_combined, targets_df, drivers_df, var_name, model_id, reference_datetime){
  
  ## join inflow data to met
  
  # forecast_drivers <- met_df |> 
  #   left_join(targets_df, by = c('date')) |> 
  #   drop_na(total_flow)
  # 
  # split <- initial_split(forecast_drivers, prop = 0.80, strata = NULL)
  # 
  # train_data <- training(split)
  # test_data <- testing(split)
  
  ## set training as all data prior to start of forecast
  # train_data <- forecast_drivers |> 
  #   dplyr::filter(date < reference_datetime)
  
  ## define folds in training data 
  folds <- vfold_cv(train_data, v = 5) # orginally set to 10
  
  # #set the recipe
  # rec <- recipe(total_flow ~ precip + sevenday_precip + doy + temperature,
  #               data = train_data)
  # 
  
  rec_preprocess <- model_recipe |> 
    step_normalize(all_numeric_predictors()) #|> 
  #step_dummy(doy)
  
  ## define model and tunining parameters (tuning 2/8 parameters right now)
  xgboost_mod <- boost_tree(tree_depth = tune(), trees = tune()) |> #, learn_rate = 0.1) |> 
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
  xgboost_inflow_fit <- fit(final_workflow, data = drivers_df)
  
  # make predictions for each ensemble member 
  forecast_precip_ens <- met_combined |> 
    dplyr::filter(variable == 'precipitation') |> 
    #summarise(precip_hourly = sum(prediction, na.rm = TRUE), .by = c("datetime","ensemble")) |> 
    mutate(date = lubridate::as_date(datetime)) |> 
    summarise(precip = sum(prediction, na.rm = TRUE), .by = c("date","ensemble")) |> 
    arrange(date, ensemble) |> 
    group_by(ensemble) |> 
    mutate(sevenday_precip = RcppRoll::roll_sum(precip, n = 7, fill = NA,align = "right")) |> 
    ungroup() |> 
    mutate(doy = lubridate::yday(date))
  
  forecast_temp_ens <- met_combined |> 
    dplyr::filter(variable == 'temperature_2m') |> 
    mutate(date = lubridate::as_date(datetime)) |> 
    summarise(temperature = median(prediction, na.rm = TRUE), .by = c("date","ensemble"))
  
  forecast_met_ens <- forecast_precip_ens |> 
    right_join(forecast_temp_ens, by = c('date',"ensemble")) |> 
    arrange(date,ensemble) #|> 
  #filter(date >= reference_datetime)
  
  # if (var_name == 'SALT'){
  #   obs_previous_df <- train_data |> 
  #     select(date,obs_previous)
  #   
  #   forecast_met_ens <- forecast_met_ens |> 
  #     full_join(obs_previous_df, by = c('date'))
  # }
  
  #make empty dataframe to store predictions
  data_build <- data.frame()
  
  for (i in unique(forecast_met_ens$ensemble)){
    
    ens_df <- forecast_met_ens |> 
      dplyr::filter(ensemble == i)
    
    ens_inflow <- predict(xgboost_inflow_fit, new_data = ens_df)
    
    ens_predictions <- cbind(ens_df,ens_inflow) |> 
      rename(prediction = .pred) |> 
      mutate(prediction = ifelse(prediction < 0, 0, prediction))
    
    
    data_build <- bind_rows(data_build,ens_predictions)
    
  }
  
  ## join observations back onto predictions
  
  ## overwrite predictions with observed data when present
  update_historical_df <- data_build |> 
    left_join(targets_df, by = c('date')) |> 
    #mutate(prediction = ifelse(!is.na(observation), observation, prediction)) |> 
    mutate(model_id = model_id) |> 
    mutate(site_id = config$location$site_id) |> 
    mutate(reference_datetime = reference_datetime) |> 
    mutate(family = 'ensemble') |> 
    mutate(variable = var_name) |> 
    mutate(flow_type = 'inflow') |> 
    mutate(flow_number = 1) |> 
    rename(parameter = ensemble, datetime = date) |> 
    select(model_id, site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_type, flow_number) |>
    filter(datetime >= as.Date(reference_datetime))
  
  return(update_historical_df)
}

# This script is a simple "process" model that assumes a travel time (T) and a rate of loss (L) 
# Therefore the calculation of flow at W at time t is
# Qdown ~ Qup(t-T) - L
# L ~ Qup(t-T) + month (as per model output from DEW)
# Travel times are estimated based on observed outpyt from Q at SA border (QSA) and L1 
# plus some estimated time between L1 and Wellington (6 days)


library(fable)

#'model_losses(model_dat = 'DEW_data/modelled_losses_DEW.csv',
#'             formula_use = "x ~ y + group", 
#'             x = 'loss', y = 'QSA', group = 'month')
#'
#' @param model_dat where are the helper data saved
#' @param formula_use what is the generic formula (can include x, y, and grouping variable for interactions)
#' @param x predictor
#' @param y response
#' @param obs_unc amount of obs uncertainty, applied as a percent of the mean loss 
#' @param group grouping var, Month probably
#' @return fitted loss model

model_losses <- function(model_dat = 'R/helper_data/modelled_losses_DEW.csv', 
                         # data used to fit model, in GL/m
                         obs_unc = 0, # how much obs uncertainty (proportion of total)
                         formula_use = 'y ~ x + group',
                         y = 'loss', x = 'QSA', group = 'month') {
  model_loss <- 
    read_csv(model_dat, show_col_types = F) |> 
    mutate(month = match(month, month.name)) |> 
    pivot_longer(starts_with('GLd_'), 
                 names_to = x,
                 values_to = y,
                 names_prefix = 'GLd_') |> 
    mutate(days_in_month = lubridate::days_in_month(month),
           {{y}} := (as.numeric(.data[[y]])/days_in_month) * 1000, # convert to ML/d from GL/m
           {{x}} := (as.numeric(.data[[x]]))*1000, # convert to ML/d from GL/d
           {{group}} := as.factor(.data[[group]])) %>% 
    select(-days_in_month) 
  
  # if there is x% error in losses
  obs_sd <- mean(model_loss$loss) * obs_unc
  
  # generate a sample to which we can fit the model based on these "obs uncertainty" samples
  model_loss_unc <- model_loss |> 
    reframe({{y}} := .data[[y]] + rnorm(n = 10, mean = 0, sd = obs_sd), .by = everything()) 
  
  # model_loss |>
  #   ggplot(aes(x = flow, y = loss,colour = as.factor(month))) +
  #   geom_point() +
  #   geom_smooth(method = 'lm')
  
  # # fit model with and without interaction
  # anova(lm(loss ~ flow + month, data = model_loss),
  #       lm(loss ~ flow * month, data = model_loss))

  # no interaction better
  formula_updated <- gsub(pattern = "x", replacement = x, 
                          x = gsub(pattern = "y", replacement = y, 
                                   x = gsub(pattern = "group", replacement = group, x = formula_use)))
  
  L_mod <- lm(as.formula(formula_updated), data = model_loss_unc)
  
  return(L_mod)
}


#' Fit a travel time model from Source data
#'
#' @param model_dat where are the helper data from Source
#' @param obs_unc how much obs uncertainty
#' @param formula_use what is the generic formula (can include x, y), different forms of a model to be converted
#' @param x predictor
#' @param y response
#'
#' @return fitted travel time model
#' 
model_traveltime <- function(model_dat = 'R/helper_data/travel_times.csv', 
                             # data used to fit model, in ML/d
                             obs_unc = 0, # how much obs uncertainty (proportion of total)
                             formula_use = 'y ~ poly(x, 3)',
                             y = 'travel_time', x = 'flow') {
  
  model_tt <- 
    read_csv(model_dat, show_col_types = F) |> 
    rename(flow = contains('_MLd'))
  
  # if there is x% error in losses
  obs_sd <- mean(model_tt$travel_time) * obs_unc
  
  # generate a sample to which we can fit the model based on these "obs uncertainty" samples
  model_tt_unc <- model_tt |> 
    reframe({{y}} := .data[[y]] + rnorm(n = 10, mean = 0, sd = obs_sd), .by = everything()) 
  
  # model_tt_unc |>
  #   ggplot(aes(x = flow_MLd, y = travel_time)) +
  #   geom_point() +
  #   geom_smooth(method = 'lm', formula = y ~ poly(x, 4, raw=TRUE))
  
  # # fit model with different polynomials
  # anova(lm(travel_time ~ poly(flow_MLd, 3), data = model_tt),
  #       lm(travel_time ~ poly(flow_MLd, 4), data = model_tt))
  
  # 4th order no better, stick with cubic
  formula_updated <- gsub(pattern = "x", replacement = x, 
                          x = gsub(pattern = "^y", replacement = y, x = formula_use))
  
  TT_mod <- lm(as.formula(formula_updated), data = model_tt_unc)
  
}


#' predict_downstream
#'
#' @returns dataframe with lagged predictors
#'
#' @param data dataframe with the column to be lagged and a datetime column, requires explicit gaps
#' @param upstream_col column name to be used as the upstream predictor
#' @param L_mod fitted loss model
#' @param loss_unc Logical, include uncertainty from loss_mod
#' @param forecast_dates 
#' @param tt_unc 
#' @param TT_mod 
#'
#' @examples
predict_downstream <- function(data, # needs a datatime column, data in ML/d, or specify units
                               forecast_dates,
                               loss_unc = T,
                               tt_unc = T,
                               upstream_col = 'QSA',
                               L_mod = L_mod,
                               TT_mod = TT_mod) {
 
   if (is.character(forecast_dates)) {
    if (forecast_dates == 'historical') {
      message('using historical time series')
      forecast_dates <- distinct(data, datetime)
    } else {
      stop('If forecast dates arent specified must used `forecast_dates = historical`')
    }
  }
  
  
  new_dat <- data |>
    mutate(#"{upstream_col}" := data1[upstream_col],
      month = as.factor(month(datetime)))
  
  if (loss_unc) {
    predicted_loss <- predict(L_mod, newdata = new_dat) + rnorm(n = nrow(new_dat), mean = 0, sd = sd(L_mod$residuals))
  } else {
    predicted_loss <- predict(L_mod, newdata = new_dat)
  }
  
  if (tt_unc) {
    predicted_tt <- as.integer(round(predict(TT_mod, newdata = new_dat) + 
                                       rnorm(n = nrow(new_dat), mean = 0, sd = sd(TT_mod$residuals))))
  } else {
    predicted_tt <- as.integer(round(predict(TT_mod, newdata = new_dat)))
    
  }
  
  upstream_lagged <- data |> 
    mutate(travel_time = predicted_tt, 
           datetime_down = datetime + days(travel_time),# on what date will this flow get downstream; lagged_upstream (Qup(t-T)), 
           loss = predicted_loss) |> # what is the predicted loss for this flow
    reframe(.by = datetime_down,
            flow = mean(flow, na.rm = T),
            loss = mean(loss, na.rm = T)) |> # takes a mean of any duplicate dates
    full_join(forecast_dates, by = join_by(datetime_down == datetime)) |> 
    arrange(datetime_down) |> 
    mutate(flow = zoo::na.approx(flow, rule = 2),
           loss = zoo::na.approx(loss, rule = 2), # interpolate missing dates
           flow_down = flow - loss)  # what is the flow downstream  given the losses; Qdown ~ Qup(t-T) - L
  
  prediction <- upstream_lagged |> 
    select(datetime_down, flow_down) |> 
    rename(datetime = datetime_down,
           prediction = flow_down)  |> 
    filter(datetime %in% forecast_dates$datetime)
  
  return(prediction)
}


#' generate_flow_inflow_fc
#'
#' @param config FLARE config file read in
#' @param lag_t number of days to lag the upstream value, can be a vector of possible lags or a single value 
#' @param L_mod loss model object generated using model_losses()
#' @param n_members how many ensemble members to generate
#' @param upstream_unit what are the units of upstream, default is MLd - if not it will do a conversion
#' @param upstream_location which gauging station to use the lags of, default is QSA
#' @param loss_unc logical, include uncertainty from L_mod
#' 
#' @returns forecast of inflow in MLd
#'
generate_flow_inflow_fc <- function(config,
                                    # lag_t,
                                    n_members = 1, 
                                    upstream_unit = 'MLd',
                                    upstream_location = 'QSA',
                                    loss_unc = T,
                                    tt_unc = T,
                                    L_mod,
                                    TT_mod) {
  # Set up
  reference_date <- as_date(config$run_config$forecast_start_datetime)
  site_id <- config$location$site_id
  start_training <- reference_date - years(5)
  horizon <- config$run_config$forecast_horizon
  end_date <- reference_date + days(horizon)
  upstream_start <- reference_date - days(60) # give buffer and use to fit RW
  
  
  # which upstream location to use
  if (!upstream_location %in% c('L1', 'QSA')) {
    stop('must use QSA or L1')
  } else if (upstream_location == 'QSA') {
    # read in recent QSA data - this is in MLd!!!
    download_WaterDataSA <- paste0("https://water.data.sa.gov.au/Export/BulkExport?DateRange=Custom&StartTime=",
                                   upstream_start, "%2000%3A00&EndTime=", reference_date, "%2000%3A00&TimeZone=9.5&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=True&IncludeGradeCodes=False&IncludeApprovalLevels=False&IncludeQualifiers=False&IncludeInterpolationTypes=False&Datasets[0].DatasetName=Discharge.Master--Daily%20Calculation--ML%2Fday%40A4261001&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=241&_=1738250581759")
    download.file(download_WaterDataSA, destfile = 'data_raw/upstream.csv')
    upstream_MLd <- read_csv('data_raw/upstream.csv', show_col_types = F,
                             skip = 5, col_names = c('datetime', 'flow')) |> 
      mutate(datetime = ymd(format(datetime, "%Y-%m-%d")))
  } else {
    # read in recent L1 data - this is in MLd!!!
    download_WaterDataSA <- paste0("https://water.data.sa.gov.au/Export/BulkExport?DateRange=Custom&StartTime=",
                                   upstream_start, "%2000%3A00&EndTime=", reference_date, "%2000%3A00&TimeZone=9.5&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=True&IncludeGradeCodes=False&IncludeApprovalLevels=False&IncludeQualifiers=False&IncludeInterpolationTypes=False&Datasets[0].DatasetName=Discharge.Master--Daily%20Read--ML%2Fday%40A4260903&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=241&_=1738251451037")
    download.file(download_WaterDataSA, destfile = 'data_raw/upstream.csv')
    upstream_MLd <- read_csv('data_raw/upstream.csv', show_col_types = F,
                             skip = 5, col_names = c('datetime', 'flow')) |> 
      mutate(datetime = ymd(format(datetime, "%Y-%m-%d")))
  }
  
  # convert units
  if (upstream_unit %in% c('m3s', 'MLd', 'GLd')) {
    if (upstream_unit == 'm3s') {
      # convert from m3/s to ML/d
      data <- data |>
        mutate("{upstream_col}" := (data[[upstream_col]] * 86.4))
    } else if (upstream_unit == 'GL/d') {
      # convert from GL/d to ML/d
      data <- data |>
        mutate("{upstream_col}" := data[[upstream_col]] * 1000)
    }
  } else {
    stop('units must be m3/s, ML/d or GL/d')
  }
  
  # Make sure the upstream_data extends as long as the forecast window - use persistence
  forecast_dates <- data.frame(datetime = seq.Date(reference_date, end_date, 'day'))
  all_upstream <- full_join(forecast_dates, upstream_MLd, by = 'datetime') |>
    arrange(datetime) #|>
  # mutate(flow = zoo::na.locf(flow))
  
  #Try with a RW model for the end of the forecast period
  RW_mod <- all_upstream |>
    tsibble::as_tsibble(index = 'datetime') |>
    tsibble::fill_gaps() |> 
    mutate(flow = zoo::na.approx(flow, na.rm = F, rule = 1:2, maxgap = 5)) |>
    na.omit() |>
    model(RW = ARIMA(flow))
  
  # calculate how long the horizon will be to estimate using the model
  model_horizon <- length(which(is.na(all_upstream$flow)))
  
  RW_fc <- RW_mod |>
    generate(h = model_horizon, times = ens_members) |>
    rename(flow = .sim, 
           parameter = .rep) |>
    as_tibble()
  
  # limit the flow at the border between entitlement (min) and eflow (max)
  eflows <- read_csv('R/helper_data/eflow.csv', show_col_types = F) |> 
    # convert to Md
    mutate(month_days = lubridate::days_in_month(match(month, month.name)), 
           eflow_MLd = (eflow_GLm / month_days) * 1000)
  
  entitlement <- read_csv('R/helper_data/entitlement_flow.csv', show_col_types = F)
  # already in MLd
  
  # combine the min/max flows
  min_max_flows <- full_join(eflows, entitlement, by = 'month') |> 
    mutate(min = ent_MLd,
           max = ent_MLd + eflow_MLd) |> 
    select(month, min, max)
  
  # Confine the RW forecast to the min/max specified by the entitlement and eflows
  bounded_RW_fc <- RW_fc |> 
    mutate(month = lubridate::month(datetime, label = T, abbr = F)) |> 
    left_join(min_max_flows, by = 'month') |>
    # ensure forecast doesn't go above/below limits
    mutate(flow = ifelse(flow > max, max, 
                         ifelse(flow < min, min, flow))) |> 
    select(datetime, parameter, flow)
  
  # Estimate the downstream flow for each ensemble member based on a randomly selected lag
  downstream_fc <- data.frame()
  
  for (m in 1:n_members) {
    
    # extract the ensemble member from the bounded RW above
    all_upstream_m <- rows_update(all_upstream,
                                  select(filter(bounded_RW_fc, parameter == m),
                                         'datetime', 'flow'),
                                  by = 'datetime')
    
    # lag_use <- lag_t[sample(length(lag_t), size = 1)] # randomly select a lag from the range given
    # Note: if you just do sample(lag_t, size = 1) it gives the wrong answer when the length(lag_T) == 1
    
    predictions <- predict_downstream(data = all_upstream_m,
                                      upstream_col = 'flow',
                                      loss_unc = loss_unc,
                                      tt_unc = tt_unc,
                                      forecast_dates = forecast_dates,
                                      L_mod = L_mod,
                                      TT_mod = TT_mod) |>
      filter(datetime %in% forecast_dates$datetime) |> 
      mutate(reference_date = as_date(reference_date),
             parameter = m - 1, 
             datetime = as_date(datetime),
             model_id = 'process_flow',
             variable = 'FLOW',
             flow_number = 1) |> 
      select(any_of(c('datetime', 'prediction', 'reference_date', 'model_id', 
                      'variable', 'flow_number', 'parameter'))) 
    
    downstream_fc <- bind_rows(downstream_fc, predictions)
  }
  
  # downstream_fc |> reframe(.by = datetime, q95 = quantile(prediction, 0.95),
  #                          q5 = quantile(prediction,0.05),
  #                          median = median(prediction)) |>
  #   ggplot(aes(x=datetime, y=median)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin = q5, ymax = q95), fill = 'blue', alpha = 0.3)
  
  return(downstream_fc)
}

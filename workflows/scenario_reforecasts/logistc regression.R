saltexport_fc |> 
  pivot_wider(values_from = prediction, 
              names_from = variable) |> 
  mutate(overflow_salt_load = (overflow_salt*overflow_flow)/1000) |> # units of salt is ppt (equal to kg/m3), units of discharge m3/day, divide by 1000 = tonnes
  reframe(.by = c(model_id, scenario, reference_date, parameter),
          total_salt_load = sum(overflow_salt_load)) |> 
  select(reference_date, parameter, total_salt_load, model_id) |> 
  pivot_wider(names_from = model_id, values_from = total_salt_load) |> 
  mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
         `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest) |> 
  pivot_longer(`raise barrages`:`lower barrages`, names_to = 'scenario', values_to = 'diff_export') |> 
  reframe(.by = c(scenario, reference_date),
          median = mean(diff_export, na.rm = T),
          quantile97.5 = quantile(diff_export, 0.975, na.rm = T),
          quantile2.5 = quantile(diff_export, 0.025, na.rm = T)) |> 
  mutate(sig_diff = ifelse(quantile2.5 < 0 & quantile97.5 > 0 , 0, 1)) |>
  full_join(inflow_flow |> 
              reframe(.by = c(parameter, scenario, reference_date),
                      median_overflow = median(prediction/1000, na = T)) |> # convert to MLday
              reframe(.by = c(scenario, reference_date),
                      median_barrage = median(median_overflow)) |> select(-scenario),
            by = 'reference_date') |> 
  ggplot(aes(x=median_barrage, y=sig_diff, colour = scenario)) +
  geom_point() +
  scale_colour_manual(values = cols_scenarios) +
  facet_wrap(~scenario) +
  theme_bw()


scenarios <- c('raise barrages', 'lower barrages')
scenarios_fitted_models <- NULL
for (i in 1:length(scenarios)) {

  
  saltexport_scenario <- saltexport_fc |> 
    pivot_wider(values_from = prediction, 
                names_from = variable) |> 
    mutate(overflow_salt_load = (overflow_salt*overflow_flow)/1000) |> # units of salt is ppt (equal to kg/m3), units of discharge m3/day, divide by 1000 = tonnes
    reframe(.by = c(model_id, scenario, reference_date, parameter),
            total_salt_load = sum(overflow_salt_load)) |> 
    select(reference_date, parameter, total_salt_load, model_id) |> 
    pivot_wider(names_from = model_id, values_from = total_salt_load) |> 
    mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
           `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest) |> 
    pivot_longer(`raise barrages`:`lower barrages`, names_to = 'scenario', values_to = 'diff_export') |> 
    reframe(.by = c(scenario, reference_date),
            median = mean(diff_export, na.rm = T),
            quantile97.5 = quantile(diff_export, 0.975, na.rm = T),
            quantile2.5 = quantile(diff_export, 0.025, na.rm = T)) |> 
    mutate(sig_diff = as_factor(ifelse(quantile2.5 < 0 & quantile97.5 > 0 , 1, 0))) |>
    # zero is within the CI 2.5-97.5 = 1 (not different from 0), if not 0, this is the opposite of what you would think, to make the plotting better
    full_join(inflow_flow |> 
                reframe(.by = c(parameter, scenario, reference_date),
                        median_inflow = median(prediction/1000, na = T)) |> # convert to MLday
                reframe(.by = c(scenario, reference_date),
                        median_inflow = median(median_inflow)) |> select(-scenario),
              by = 'reference_date') |> 
    filter(scenario == scenarios[i])
  
  
  scenario_model <- glm(sig_diff ~ median_inflow, family="binomial", data=saltexport_scenario)
  print(summary(scenario_model))
  
  # solve equation at 0.5
  b0 <- coefficients(scenario_model)[1]
  b1 <- coefficients(scenario_model)[2]
  P <- 0.5 
  inflow_P <- (-1*(log((1/P) - 1)) - b0)/ b1
  message(scenarios[i], " P = 0.5 ... ", inflow_P)

  
  print(anova(scenario_model,     
              test="Chisq"))
  
  barrage_flows <- seq(min(saltexport_scenario$median_inflow), max(saltexport_scenario$median_inflow), length.out = 100)
  
  probs <- predict(scenario_model,
                   newdata = data.frame(median_inflow = barrage_flows),
                   type = "response",
                   se.fit = TRUE)
  
  fitted_model <- data.frame(median_inflow = barrage_flows, 
                             fit = probs$fit,
                             upper = probs$fit + probs$se.fit * 1.96, # 95% confidence interval
                             lower = probs$fit - probs$se.fit * 1.96,
                             scenario = scenarios[i]) # 95% confidence interval
  
  scenarios_fitted_models <- bind_rows(fitted_model, scenarios_fitted_models)
}

saltexport_fc |> 
  pivot_wider(values_from = prediction, 
              names_from = variable) |> 
  mutate(overflow_salt_load = (overflow_salt*overflow_flow)/1000) |> # units of salt is ppt (equal to kg/m3), units of discharge m3/day, divide by 1000 = tonnes
  reframe(.by = c(model_id, scenario, reference_date, parameter),
          total_salt_load = sum(overflow_salt_load)) |> 
  select(reference_date, parameter, total_salt_load, model_id) |> 
  pivot_wider(names_from = model_id, values_from = total_salt_load) |> 
  mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
         `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest) |> 
  pivot_longer(`raise barrages`:`lower barrages`, names_to = 'scenario', values_to = 'diff_export') |> 
  reframe(.by = c(scenario, reference_date),
          median = mean(diff_export, na.rm = T),
          quantile95 = quantile(diff_export, 0.95, na.rm = T),
          quantile5 = quantile(diff_export, 0.05, na.rm = T)) |> 
  mutate(sig_diff = ifelse(quantile5 < 0 & quantile95 > 0 , 1, 0))|> # reversed 1=
  full_join(inflow_flow |> 
              reframe(.by = c(parameter, scenario, reference_date),
                      median_inflow = median(prediction/1000, na = T)) |> # convert to MLday
              reframe(.by = c(scenario, reference_date),
                      median_inflow = median(median_inflow)) |> select(-scenario),
            by = 'reference_date') |> 
  ggplot() +
  geom_point(aes(x=median_inflow, y=sig_diff, colour = scenario)) +
  scale_colour_manual(values = cols_scenarios) +
  facet_wrap(~scenario, nrow = 2) +
  geom_line(data = scenarios_fitted_models, aes(x=median_inflow, y = fit, colour = scenario))  +
  theme_bw() +
  geom_hline(yintercept = 0.5, alpha = 0.7, linewidth = 0.4, linetype = 'dashed') +
  scale_y_continuous(breaks = c(0,0.5, 1), labels = c('TRUE',"0.5", 'FALSE'), name = 'Different to reference scenario?') +
  scale_x_continuous(breaks = seq(0, 30000, 5000), name = 'Median inflow (ML/day)')


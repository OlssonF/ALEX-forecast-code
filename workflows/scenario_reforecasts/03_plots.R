#--------------------------------------#
## Project: AlEX-reforecasts
## Script purpose: generate plots for manuscript and supplementary information
## Date: 2025-04-08
## Author: Freya Olsson
#--------------------------------------#

# package loading ---------------------------
library(tidyverse)
library(arrow)
library(ggpubr)

# Set up -------------------------------------------------------
## read in data ------------------------------------------------
model_ids <- c('reference' = 'glm_flare_v3_crest',
               'raise' = 'glm_flare_v3_crest_up_0.1',
               'lower' = 'glm_flare_v3_crest_down_0.1')
scenarios <- names(model_ids)
site_id <- 'ALEX'
forecast_horizon <- 30

starting_date <- as_date("2023-07-01") # from the start of a water year
ref_dates <- seq.Date(starting_date, by = 7, length.out = 92)

forecasts <- open_dataset('forecasts/parquet/') |> 
  filter(site_id == site_id,
         model_id %in% model_ids,
         reference_date %in% ref_dates) |> 
  collect() |> 
  mutate(scenario = ifelse(model_id == 'glm_flare_v3_crest',
                           'reference',
                           ifelse(model_id == "glm_flare_v3_crest_down_0.1",
                                  'lower barrages', "raise barrages")))

scores <- open_dataset('scores/site_id=ALEX/') |> 
  filter(site_id == site_id,
         model_id %in% c('glm_flare_v3_crest', 'climatology', 'persistenceRW'),
         reference_date %in% ref_dates) |> 
  collect() 

## set aesthetics -----------------------------------------------

var.labs <- c("lake level (m, above AHD)", "in-situ lake salinity (ppt)", "in-situ lake water temperature (°C)",  "temperature (°C)", 'discharge (m3/s)', 'EC (uS/cm)')
names(var.labs) <- c("depth", "salt", 'temperature', 'temp', 'flow', 'EC')

mod.labs <- c("day-of-year null model", "persistence null model", "FLARE")
names(mod.labs) <- c("climatology","persistenceRW", "glm_flare_v3_crest")

flow.labs <- c("high flow", "low flow", "normal flow")
names(flow.labs) <- c("high_flow", "low_flow", "normal_flow")

cols_mods <- viridisLite::rocket(n=3, begin = 0.1, end = 0.6)#"#261433FF" "#8B1D5BFF" "#E83F3FFF"
cols_scenarios <- c(viridisLite::plasma(n = 3, begin = 0.9, end = 0.7)[c(1,3)], 'grey40') # "#FCCE25FF" "#F1844BFF" "grey"
cols_flowlevels <- viridisLite::viridis(n = 3, begin = 0.1, end = 0.95)# "#482576FF" "#1F968BFF" "#DEE318FF"
cols_inflowens <-  viridisLite::mako(n = 2, begin = 0, end = 0.5) # "#0B0405FF" "#357BA2FF"

names(cols_mods) <- c("climatology","persistenceRW", "glm_flare_v3_crest")
names(cols_scenarios) <- c("raise barrages", "lower barrages", 'reference')
names(cols_flowlevels) <- c('high_flow', "normal_flow", "low_flow" )
names(cols_inflowens) <- c('max', 'min')
# Main text figures ---------------------------------------------------

# Figure 3 - Observations -------
in-situ_obs <- read_csv('targets/ALEX/ALEX-targets-in-situ.csv') |> 
  mutate(variable = ifelse(variable == 'salt', 'in-situ salinity', 
                           ifelse(variable == 'temperature', 'in-situ temperature', variable)),
         observation = ifelse(variable == 'depth', observation - 5.3, observation))

inflow_obs <- read_csv('targets/ALEX/ALEX-targets-inflow.csv') |>
  filter(inflow_name == 'murray', variable == 'FLOW') |> 
  mutate(variable = ifelse(variable == 'FLOW', 'inflow', variable))

outflow_obs <- read_csv('targets/ALEX/ALEX-targets-outflow.csv') |> 
  filter(variable == 'FLOW') |> 
  mutate(variable = ifelse(variable == 'FLOW', 'barrage outflow', 
                           ifelse(variable == 'salt', 'outflow salt', variable)))


all_obs <- bind_rows(in-situ_obs, inflow_obs, outflow_obs)|> 
  filter(between(datetime, 
                 as_datetime('2023-07-01'),
                 as_datetime('2025-05-01'))) |> 
  select(-depth) |> 
  pivot_wider(values_from = observation, names_from = variable) |> 
  mutate(`salt export` = (`in-situ salinity`*`barrage outflow`),
         `electrical conductivity` = gsw::gsw_C_from_SP(SP = `in-situ salinity`, t = `in-situ temperature`, p = 0)*1000) |> 
  pivot_longer(depth:`electrical conductivity`, names_to = 'variable', values_to = 'observation') 

source('R/make_obs_plot.R')

plot_vars <- c("inflow","depth", "in-situ temperature",  "barrage outflow", "salt export")
plot_labels <- c("calculated flow\nat SA border (ML/day)", 
                 "lake level\n(m, above AHD)",
                 "in-situ lake water temperature (C)",
                 "combined barrage outflow\nfrom lake (ML/day)",
                 "salt export from lake\n(tonnes/day)")

obs_plots <- purrr::pmap(list(plot_vars,
                              plot_labels),
                         ~make_obs_plot(df = all_obs, 
                                        plot_var = ..1, plot_label = ..2)) 

salinity_obs_plot <-
  all_obs |> 
  filter(variable %in% c("in-situ salinity", "electrical conductivity")) |> 
  pivot_wider(names_from = variable, values_from = observation) |> 
  ggplot(aes(x = datetime)) +
  geom_point(aes(y=`in-situ salinity`), size = 0.5) +
  geom_point(aes(y=`electrical conductivity`/1000), size = 0.5, colour = 'red') +
  scale_x_datetime(breaks = as_datetime(c("2023-07-01", "2024-01-01", "2024-07-01",
                                          "2025-01-01")),
                   date_labels = "%b %Y") +
  theme_bw() + labs(x='Date') +
  scale_y_continuous(name = "\nin-situ lake salinity (ppt)",
                     sec.axis = sec_axis(~.*1000, name=expression("electrical conductivity (" * mu ~ "S/cm)"))) +
  theme(axis.title.y.right = element_text(color = 'red', vjust = 132, angle = 90),
        axis.text.y.right = element_text(color = 'red', vjust = 134, hjust = 0.5, angle = 90),
        axis.ticks.y.right = element_blank()) +
  annotate(geom = 'segment', yend = 1, y = 0, x = as_datetime('2022-12-10'), colour = 'red') +
  annotate(geom = 'point', y = c(0, 0.25, 0.5, 0.75, 1), x = as_datetime('2022-12-08'), shape = 45, size = 4.5, colour = 'red') +
  coord_cartesian(ylim = c(0.049, 0.951), clip = "off", 
                  xlim = c(as_datetime('2023-07-01'), as_datetime('2025-05-01')))


obs_plots_arranged <- ggarrange(obs_plots[[1]], obs_plots[[2]],
                                obs_plots[[3]], salinity_obs_plot,
                                obs_plots[[4]], obs_plots[[5]], 
                                nrow = 3, ncol = 2, align = 'hv', labels = c("A)","B)","C)","D)","E)","F)"), hjust = -1.2)

ggsave(plot = obs_plots_arranged,
       filename = 'plots/ms/Figure_3.png', 
       height = 20, width = 22.5, units = 'cm') # if you change the size you will need to sort out the red labels above!
# Figure 4 - in-situ forecast evaluation -------------------------------------
eval_vars <- c('temperature', 'salt', 'depth')
eval_depths <- 0.5

eval_plot <- 
  scores |>
  filter(variable %in% eval_vars,
         depth %in% eval_depths | is.na(depth),
         horizon > 0) |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime)),
         sq_error = (median - observation)^2) |> 
  reframe(.by = all_of(c('horizon', 'variable', 'depth', 'model_id')),
          median_crps = median(crps, na.rm = T),
          rmse = sqrt(mean(sq_error))) |> 
  ggplot(aes(x=horizon, y=median_crps, colour = model_id, linetype = model_id)) +
  # geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
  theme_bw() +
  facet_wrap(~variable, nrow = 1, scales = 'free', labeller = labeller(variable = var.labs)) +
  labs(y = 'Median CRPS', x = "Horizon (days)") +
  scale_y_continuous(limits = ~ c(min(.x), max(.x)*1.9)) +
  scale_colour_manual(values = cols_mods, labels = mod.labs, breaks = names(mod.labs), name = 'Forecast model') +
  scale_linetype_manual(values = c('longdash', 'dashed', 'solid'), labels = mod.labs, breaks = names(mod.labs), name = 'Forecast model') +
  theme(legend.position = 'top')

ggsave(plot = eval_plot,
       filename = 'plots/ms/Figure_4.png', 
       height = 8, width = 18, units = 'cm')

# Figure 5 - example forecast ---------------------------------------------------
example_ref_date <- "2024-03-23"
plot_vars <- c('crest_elev', 'overflow_flow', 'overflow_salt', 'salt', 'temperature', 'depth')
plot_depths <- c(NA, 0.5)

example_fc <- forecasts |> ungroup() |> 
  filter(reference_date %in% example_ref_date,
         variable %in% plot_vars,
         depth %in% plot_depths,
         prediction != -9999) 

# calculate the example
example_saltexport_fc <- example_fc |>
  filter(variable %in% c("overflow_salt", "overflow_flow")) |> 
  pivot_wider(values_from = prediction, names_from = variable) |> 
  mutate(salt_export = (overflow_salt*overflow_flow/1000)) |> 
  # units of salt is ppt (equal to kg/m3), units of discharge ML/d = tonnes/day
  pivot_longer(overflow_flow:salt_export, names_to = 'variable', values_to = 'prediction')  |> 
  filter(variable == 'salt_export')


example_fc_plot <- example_fc |> 
  bind_rows(example_saltexport_fc) |> 
  select(datetime, reference_date, variable, parameter, prediction, model_id, scenario) |> 
  mutate(prediction = ifelse(variable == 'depth', prediction - 5.3, 
                             ifelse(variable == "overflow_flow", prediction/1000, prediction))) |> 
  reframe(.by = c("datetime", "variable", "scenario","reference_date"),
          median = median(prediction, na.rm = T),
          q97.5 = quantile(prediction, 0.975, na.rm = T),
          q02.5 = quantile(prediction, 0.025, na.rm = T)) |> 
  filter(variable != "overflow_salt") |> # don't need to plot this
  mutate(variable = factor(variable,
                           levels = c("crest_elev", "depth",
                                      "temperature", "salt",
                                      "overflow_flow", "salt_export"),
                           labels = c("A) barrage height (m, above AHD)", "B) lake level (m, above AHD)",
                                      "C) in-situ lake water temperature (C)", "D) in-situ lake salinity (ppt)",
                                      "E) barrage outflow from lake (ML/day)", "F) salt export from lake (tonnes/day)")),
         scenario = factor(scenario,
                           levels = c("raise barrages", "lower barrages", "reference"))) |> 
  mutate(median = ifelse(datetime < as_datetime(reference_date) & scenario != "reference", NA, median),
         q97.5 = ifelse(datetime < as_datetime(reference_date) & scenario != "reference", NA, q97.5),
         q02.5 = ifelse(datetime < as_datetime(reference_date) & scenario != "reference", NA, q02.5)) |> 
  ggplot(aes(x=datetime, y=median)) +
  geom_ribbon(aes(ymax = q97.5, ymin = q02.5, fill = scenario), alpha = 0.1) +
  geom_line(aes(y =q97.5, colour = scenario), linetype = 'dashed', alpha = 0.7) +
  geom_line(aes(y =q02.5, colour = scenario), linetype = 'dashed', alpha =0.7) +
  geom_line(aes(colour = scenario)) +
  facet_wrap(~variable, scales = 'free_y',nrow = 3) +
  geom_vline(aes(xintercept = as_datetime(example_ref_date)), linetype = 'dashed') +
  theme_bw() +
  labs(y="Prediction with 95% predictive intervals", x = "Date") +
  scale_colour_manual(values = cols_scenarios) +
  scale_fill_manual(values = cols_scenarios)

ggsave(plot = example_fc_plot,
       filename = 'plots/ms/Figure_5.png', 
       height = 18, width = 20.5, units = 'cm')
# Figure 6 -comparison of the barrage scenarios ==================
# on salt export
saltexport_fc <- forecasts |> ungroup() |> 
  filter(variable %in% c('overflow_salt', 'overflow_flow'),
         datetime > as_date(reference_date)) 

salt_scenario <- saltexport_fc |> 
  pivot_wider(values_from = prediction, 
              names_from = variable) |> 
  mutate(overflow_salt_load = (overflow_salt*overflow_flow)/1000) |> # units of salt is ppt (equal to kg/m3), units of discharge m3/day, divide by 1000 = tonnes
  reframe(.by = c(model_id, scenario, reference_date, parameter),
          total_salt_load = sum(overflow_salt_load)) |> 
  reframe(.by = c(model_id, scenario, reference_date),
          median = median(total_salt_load, na.rm = T),
          quantile95 = quantile(total_salt_load, 0.95, na.rm = T),
          quantile5 = quantile(total_salt_load, 0.05, na.rm = T)) |> 
  ggplot(aes(x=as_date(reference_date), y= median)) +
  geom_line(aes(colour = scenario)) +
  geom_ribbon(aes(ymin = quantile5, ymax = quantile95, fill = scenario), alpha = 0.3) +
  theme_bw() +
  labs(y = "Total salt export\nover forecast horizon (tonnes)", x = 'Forecast generation date')+
  scale_colour_manual(values = cols_scenarios) +
  scale_fill_manual(values = cols_scenarios) +
  scale_x_date(breaks = as_date(c("2023-07-01", "2024-01-01", "2024-07-01",
                                  "2025-01-01", "2025-06-01")),
               date_labels = "%d %b %Y") 



# difference between scenarios
salt_scenario_diff <- saltexport_fc |> 
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
          median = median(diff_export, na.rm = T),
          quantile95 = quantile(diff_export, 0.95, na.rm = T),
          quantile5 = quantile(diff_export, 0.05, na.rm = T))  |> 
  ggplot(aes(x=as_date(reference_date))) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_ribbon(aes(ymin = quantile5,ymax= quantile95, fill = scenario),
              alpha = 0.3)  +
  geom_line(aes(y=median, colour = scenario))  +
  scale_y_continuous(expand = c(0.01,0), breaks = seq(-40000, 40000, 10000)) +
  scale_colour_manual(values = cols_scenarios) +
  scale_fill_manual(values = cols_scenarios) +
  scale_x_date(breaks = as_date(c("2023-07-01", "2024-01-01", "2024-07-01",
                                  "2025-01-01", "2025-06-01")),
               date_labels = "%d %b %Y") +
  theme_bw() +
  labs(y = "Difference from reference in\ncumulative salt export (tonnes)", x = 'Forecast generation date') 

# comparison of the barrage scenarios on depth
depth_fc <- forecasts |> ungroup() |> 
  filter(variable %in% c('depth'),
         datetime > as_date(reference_date)) 

depth_scenario <- depth_fc |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_date))) |> 
  filter(horizon %in% c(1,30)) |> 
  select(reference_date, parameter, scenario, model_id, horizon, prediction) |> 
  pivot_wider(names_from = horizon,
              names_prefix = "h_",
              values_from = prediction) |> 
  mutate(height_change = h_30 - h_1) |> 
  reframe(.by = c(model_id, scenario, reference_date),
          median = median(height_change, na.rm = T),
          quantile95 = quantile(height_change, 0.95, na.rm = T),
          quantile5 = quantile(height_change, 0.05, na.rm = T)) |>   
  ggplot(aes(x=as_date(reference_date),y=median)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_ribbon(aes(ymax = quantile95, ymin = quantile5, fill = scenario), alpha = 0.3) +
  geom_line(aes(colour = scenario)) +
  theme_bw() +
  labs(y = "Change in lake level\nover forecast horizon (m)", x = 'Forecast generation date')+
  scale_colour_manual(values = cols_scenarios) +
  scale_fill_manual(values = cols_scenarios)  +
  scale_x_date(breaks = as_date(c("2023-07-01", "2024-01-01", "2024-07-01",
                                  "2025-01-01", "2025-06-01")),
               date_labels = "%d %b %Y")

depth_scenario_diff <- depth_fc |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_date))) |> 
  filter(horizon %in% c(1,30)) |> 
  select(reference_date, parameter, model_id, scenario, horizon, prediction) |> 
  pivot_wider(names_from = horizon,
              names_prefix = "h_",
              values_from = prediction) |> 
  mutate(height_change = h_30 - h_1) |> 
  select(reference_date, model_id, parameter, height_change) |> 
  pivot_wider(names_from = model_id, values_from = height_change) |> 
  mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
         `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest) |> 
  pivot_longer(`raise barrages`:`lower barrages`, names_to = 'scenario', values_to = 'diff_height') |> 
  reframe(.by = c(scenario, scenario, reference_date),
          median = median(diff_height, na.rm = T),
          quantile95 = quantile(diff_height, 0.95, na.rm = T),
          quantile5 = quantile(diff_height, 0.05, na.rm = T)) |>   
  ggplot(aes(x=as_date(reference_date),y=median)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_ribbon(aes(ymax = quantile95, ymin = quantile5, fill = scenario),
              alpha = 0.3) +
  geom_line(aes(colour = scenario)) +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_colour_manual(values = cols_scenarios) +
  scale_fill_manual(values = cols_scenarios)  +
  scale_x_date(breaks = as_date(c("2023-07-01", "2024-01-01", "2024-07-01",
                                  "2025-01-01", "2025-06-01")),
               date_labels = "%d %b %Y") +
  theme_bw() +
  labs(y = "Difference from reference in\nlake level change (m)", x = 'Forecast generation date') 


inflow_flow <- forecasts |> ungroup() |> 
  filter(variable %in% c('inflow'),
         scenario == "reference",
         datetime > as_date(reference_date)) 

fc_inflow_flow <- inflow_flow |> 
  reframe(.by = c(parameter, scenario, reference_date),
          mean_overflow = mean(prediction/1000, na = T)) |> # convert to MLday
  reframe(.by = c(scenario, reference_date),
          median = median(mean_overflow),
          quantile95 = quantile(mean_overflow, 0.95, na.rm = T),
          quantile5 = quantile(mean_overflow, 0.05, na.rm = T)) |> 
  ggplot(aes(x=as_date(reference_date),y=median)) +
  geom_ribbon(aes(ymax = quantile95, ymin = quantile5),
              alpha = 0.3) +
  geom_line() +
  scale_y_continuous(expand = c(0.01,0))  +
  scale_x_date(breaks = as_date(c("2023-07-01", "2024-01-01", "2024-07-01",
                                  "2025-01-01", "2025-06-01")),
               date_labels = "%d %b %Y") +
  theme_bw() +
  labs(y = "Inflow (ML/day)", x = 'Forecast generation date') 

scenario_comp_plot <- ggpubr::ggarrange(plotlist = list(salt_scenario,depth_scenario,
                                                        fc_inflow_flow, fc_inflow_flow,
                                                        salt_scenario_diff, depth_scenario_diff), 
                                        nrow = 3, ncol = 2, align = 'hv', 
                                        heights = c(1,0.5,1),
                                        common.legend = T, 
                                        labels = c("A)","B)","C)","D)","E)","F)"), 
                                        hjust = -2)

ggsave(plot = scenario_comp_plot,
       filename = 'plots/ms/Figure_6.png', 
       height = 20, width = 22.5, units = 'cm')
# Figure 7 inflow vs outflow scenario importance ------------------------------
# identify high/low inflow ensemble members ------------------#
# for each reference_date find the parameters (ensemble members that are most closely related to )
min_inflow_param <- forecasts |> ungroup() |> 
  filter(variable == 'inflow', 
         scenario == 'reference') |> 
  mutate(prediction == ifelse(prediction == -9999, 0, prediction)) |> # means there is no flow
  reframe(.by = c('reference_date', 'parameter'),
          tot_flow = sum(prediction, na.rm = T)) |>
  group_by(reference_date) |> 
  slice_min(tot_flow) |> 
  mutate(flow_extreme = "min") |> 
  select(-tot_flow)


max_inflow_param <- forecasts |> ungroup() |> 
  filter(variable == 'inflow', 
         scenario == 'reference') |> 
  mutate(prediction == ifelse(prediction == -9999, 0, prediction)) |> # means there is no flow
  reframe(.by = c('reference_date', 'parameter'),
          tot_flow = sum(prediction, na.rm = T)) |>
  group_by(reference_date) |>
  slice_max(tot_flow) |> 
  mutate(flow_extreme = "max") |> 
  select(-tot_flow)


flow_extreme_scenarios <- max_inflow_param |> 
  bind_rows(min_inflow_param)
#-------------------------------------------#

# Filter the forecasts -----------------------# 
# to just the ensemble members and scenarios of interest
flow_extreme_fc <- forecasts |> 
  filter(variable %in% c('temperature', 'salt', 'depth'),
         scenario != 'reference', # take only the raise and lower
         datetime > as_date(reference_date),
         depth %in% c(NA, 0.5)) |> 
  inner_join(flow_extreme_scenarios, by = join_by(parameter, reference_date)) 
#--------------------------------------------#

# 1. median predictions by reference_date/scenario/inflow
cols <- c('min' = "#FCCE25FF",
          'max' = "#F1844BFF",
          "raise barrages" = "#0B0405FF", 
          "lower barrages" = "#357BA2FF" )
line_types <- c('min' = 'solid', 'max'= 'solid', 
                "raise barrages" = "longdash", "lower barrages" = 'longdash')
group_labs <- c('min' = 'Scenario effect under minimum inflow', 
                'max'= 'Scenario effect under maximum inflow',
                "raise barrages" = "Inflow effect under raised barrages",
                "lower barrages" = "Inflow effect under lower barrages")

p1 <- flow_extreme_fc  |>
  select(reference_date, datetime, prediction, scenario, flow_extreme, variable)  |>
  reframe(.by = c(reference_date, flow_extreme, scenario, variable), 
          med_prediction = median(prediction)) |>
  ggplot(aes(x=as_date(reference_date), y = med_prediction, colour = scenario, linetype = flow_extreme))+
  geom_line() +
  facet_wrap(~variable, scales = 'free', nrow = 3, labeller = labeller(variable = var.labs)) +
  scale_colour_manual(values = cols_scenarios, name = "") +
  scale_linetype_manual(values = c('dashed', 'solid'), name = "", 
                        breaks = c('max', 'min'), 
                        labels = c('maximum forecasted inflow', 'minimum forecasted inflow'))  +
  theme_bw(base_size = 14) +
  theme(legend.position = 'right') +
  labs(y = 'Forecast horizon median prediction', x = 'Forecast generation date')


# 2. median prediction differences

p2 <- bind_rows(flow_extreme_fc  |> 
                  select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
                  pivot_wider(names_from = scenario, values_from = prediction) |> 
                  mutate(diff = abs(`lower barrages` - `raise barrages`),
                         group = flow_extreme) |> 
                  reframe(.by = c(reference_date, group, variable), 
                          median_diff = median(diff)),
                
                flow_extreme_fc  |> 
                  select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
                  pivot_wider(names_from = flow_extreme, values_from = prediction) |> 
                  mutate(diff = abs(min - max), 
                         group = scenario) |> 
                  reframe(.by = c(reference_date, group, variable), 
                          median_diff = median(diff))) |> 
  ggplot(aes(x=as_date(reference_date), y=median_diff, colour = group, linetype = group))+
  geom_line() +
  facet_wrap(~variable, scales = 'free', nrow = 3, labeller = labeller(variable = var.labs)) +
  scale_colour_manual(values = cols, name = '',
                      breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                      labels = group_labs) +
  scale_linetype_manual(values = line_types, name = '',
                        breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                        labels = group_labs) +
  theme_bw(base_size = 14) +
  theme(legend.position = 'none') +
  labs(y = 'Absolute difference in prediction', x = 'Forecast generation date')



# 3. the violins of the same data
p3 <- 
  bind_rows(flow_extreme_fc  |> 
              select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
              pivot_wider(names_from = scenario, values_from = prediction) |> 
              mutate(diff = abs(`lower barrages` - `raise barrages`),
                     group = flow_extreme) |> 
              reframe(.by = c(reference_date, group, variable), 
                      median_diff = median(diff)),
            
            flow_extreme_fc  |> 
              select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
              pivot_wider(names_from = flow_extreme, values_from = prediction) |> 
              mutate(diff = abs(min - max), 
                     group = scenario) |> 
              reframe(.by = c(reference_date, group, variable), 
                      median_diff = median(diff))) |> 
  mutate(group = factor(group, levels =  c('min', 'max', 'lower barrages', 'raise barrages')),
         effect = ifelse(group %in% c('min', 'max'), 'scenario effect', 'inflow effect'),
         combined_group = factor(paste(effect, group, sep = ': '),
                                 levels = c('scenario effect: min', 'scenario effect: max',
                                            'inflow effect: lower barrages', 'inflow effect: raise barrages'),
                                 labels = c('scenario effect: min inflow', 'scenario effect: max inflow',
                                            'inflow effect: lower barrages', 'inflow effect: raise barrages'))) |>
  ggplot(aes(x=combined_group, y=median_diff, fill = group, linetype = group)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~variable, scales = 'free', nrow = 3, labeller = labeller(variable = var.labs)) +
  scale_fill_manual(values = cols, name = '',
                    breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                    labels = group_labs) +
  scale_linetype_manual(values = line_types, name = '',
                        breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                        labels = group_labs) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_bw(base_size = 14) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  labs(y = 'Absolute difference in prediction', x ='')



legend_plot <- get_legend(p2 + theme(legend.position = "top") + guides(colour=guide_legend(nrow=2),
                                                                       linetype=guide_legend(nrow=2)))
scenario_effect_plot <- ggarrange(ggarrange(NULL, legend_plot, widths = c(0.8, 1), nrow = 1, ncol = 2),
                                  ggarrange(p1, p2, p3, nrow = 1, ncol = 3, widths = c(1.3,0.9,0.7), align = 'h',
                                            labels = c('A', 'B'), hjust = -2), 
                                  nrow=2, ncol = 1, heights = c(0.1, 1))

ggsave(plot = scenario_effect_plot,
       filename = 'plots/ms/Figure_7.png', 
       height = 20, width = 35, units = 'cm')


# Supplementary information figures ----------------------------------

# Figure S6 - raw evaluation values -----------
eval_vars <- c('temperature', 'salt', 'depth')
eval_depths <- 0.5

scores |>
  filter(variable %in% eval_vars,
         depth %in% eval_depths | is.na(depth),
         horizon > 0) |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime)),
         sq_error = (median - observation)^2) |> 
  reframe(.by = all_of(c('horizon', 'variable', 'depth', 'model_id')),
          median_crps = median(crps, na.rm = T),
          rmse = sqrt(mean(sq_error))) |> 
  ggplot(aes(x=horizon, y=median_crps, colour = model_id)) +
  geom_line() +
  # geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
  theme_bw() +
  facet_wrap(~variable, nrow = 1, scales = 'free', labeller = labeller(variable = var.labs)) +
  labs(y = 'Median CRPS', x = "Horizon (days)") +
  scale_colour_manual(values = cols_mods, labels = mod.labs, breaks = names(mod.labs), name = 'Forecast model') +
  theme(legend.position = 'top')


# Figure S3 - evaluate overflow/outflow ---------------------------
DA_period_overflow <- forecasts |>
  dplyr::filter(model_id == 'glm_flare_v3_crest',
                datetime < as_date(reference_date),
                variable == 'overflow_flow',
                !is.na(prediction)) |> 
  mutate(prediction = prediction/1000) # convert from m3/d to ML/day 

# read in observations of the calculated barrage outflow
obs_outflow <- read_csv('targets/ALEX/ALEX-targets-outflow.csv', show_col_types = F) |> 
  filter(variable == 'FLOW') |> 
  mutate(variable = 'overflow_flow', # to match the model output
         observation = observation * 86.4) # convert to ML/d

# plot comparison
DA_period_overflow |> ungroup() |> 
  reframe(.by = c("datetime", "reference_datetime", "reference_date"),
          median = median(prediction, na.rm = T),
          q97.5 = quantile(prediction, 0.975, na.rm = T),
          q02.5 = quantile(prediction, 0.025, na.rm = T)) |> 
  left_join(obs_outflow, by = 'datetime') |> 
  ggplot(aes(x=datetime, y=observation)) +
  geom_point() +
  geom_line(aes(y = median)) +
  geom_ribbon(aes(ymax = q97.5, ymin = q02.5), alpha = 0.3) +
  scale_x_datetime(breaks = "2 months", date_labels = "%d %b %Y") +
  labs(y = "Discharge of flow through barrages (ML/day)", x = 'Date') +
  theme_bw()

# Figure S4 - high/low flow dates --------------
# define categories for each forecast date 
inflow_percentiles <- forecasts |> ungroup() |> 
  filter(scenario == 'reference', 
         variable == 'inflow',
         datetime > as_date(reference_date)) |> 
  reframe(.by = reference_date,
          mean_inflow = mean(prediction)) |> 
  reframe(high_flow = quantile(mean_inflow, 0.75),
          low_flow = quantile(mean_inflow, 0.25))

flow_cats <- forecasts |> ungroup() |> 
  filter(scenario == 'reference', 
         variable == 'inflow',
         datetime > as_date(reference_date)) |> 
  reframe(.by = reference_date,
          mean_inflow = mean(prediction)) |> 
  mutate(flow_category = ifelse(mean_inflow >= inflow_percentiles$high_flow, 'high_flow', 
                                ifelse(mean_inflow <= inflow_percentiles$low_flow, 'low_flow',
                                       'normal_flow')))

inflow_flow <- forecasts |> ungroup() |> 
  filter(variable %in% c('inflow'),
         scenario == "reference",
         datetime > as_date(reference_date)) 

inflow_flow |> 
  reframe(.by = c(parameter, scenario, reference_date),
          mean_overflow = mean(prediction/1000, na = T)) |> # convert to MLday
  reframe(.by = c(scenario, reference_date),
          mean = mean(mean_overflow)) |> 
  ggplot(aes(x=as_date(reference_date),y=mean)) +
  geom_line() +
  scale_y_continuous(expand = c(0.01,0))  +
  scale_x_date(breaks = as_date(c("2023-07-01", "2024-01-01", "2024-07-01",
                                  "2025-01-01", "2025-06-01")),
               date_labels = "%d %b %Y") +
  theme_bw() +
  labs(y = "Inflow (ML/day)", x = 'Forecast generation date') +
  geom_vline(data = flow_cats, aes(xintercept = as_date(reference_date), colour = flow_category)) +
  scale_colour_manual(values = cols_flowlevels, labels = flow.labs, breaks = names(flow.labs), name = 'Flow category')


# Figure S8 - scenario differences salt and temp-----
ggarrange(forecasts |> ungroup() |> 
            filter(variable == 'temperature',
                   depth == 0.5,
                   datetime > as_date(reference_date)) |> 
            select(datetime, reference_date, variable, parameter, prediction, depth, model_id) |> 
            pivot_wider(names_from = model_id, values_from = prediction) |> 
            mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
                   `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest)  |> 
            select(-starts_with('glm')) |> 
            pivot_longer(cols = `raise barrages`:`lower barrages`,  names_to = 'scenario', values_to = "difference") |>
            mutate(horizon = as.numeric(datetime - as_date(reference_date)))  |> 
            reframe(.by = c('horizon', 'scenario'),
                    quantile5 = quantile(difference, 0.05, na.rm = T),
                    quantile95 = quantile(difference, 0.95, na.rm = T),
                    median = median(difference, na.rm = T))|> 
            ggplot(aes(x=horizon)) +
            geom_hline(yintercept = 0)+
            geom_ribbon(aes(ymin = quantile5,ymax= quantile95, fill = scenario),
                        alpha = 0.1)  +
            geom_line(aes(y=median, colour = scenario))  +
            scale_y_continuous(expand = c(0.01,0)) +
            theme_bw() +
            labs(y= "Temperature difference from\nreference (°C)", x = 'Horizon (days)') +
            scale_colour_manual(values = cols_scenarios) +
            scale_fill_manual(values = cols_scenarios), 
          
          forecasts |> ungroup() |> 
            filter(variable == 'salt',
                   depth == 0.5,
                   datetime > as_date(reference_date)) |> 
            select(datetime, reference_date, variable, parameter, prediction, depth, model_id) |> 
            pivot_wider(names_from = model_id, values_from = prediction) |> 
            mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
                   `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest)  |> 
            select(-starts_with('glm')) |> 
            pivot_longer(cols = `raise barrages`:`lower barrages`,  names_to = 'scenario', values_to = "difference") |>
            mutate(horizon = as.numeric(datetime - as_date(reference_date)))  |> 
            reframe(.by = c('horizon', 'scenario'),
                    quantile5 = quantile(difference, 0.05, na.rm = T),
                    quantile95 = quantile(difference, 0.95, na.rm = T),
                    median = median(difference, na.rm = T))|> 
            ggplot(aes(x=horizon)) +
            geom_hline(yintercept = 0)+
            geom_ribbon(aes(ymin = quantile5,ymax= quantile95, fill = scenario),
                        alpha = 0.1)  +
            geom_line(aes(y=median, colour = scenario))  +
            scale_y_continuous(expand = c(0.01,0)) +
            theme_bw() +
            labs(y= "Salinity difference from\nreference (ppt)", x = 'Horizon (days)') +
            scale_colour_manual(values = cols_scenarios) +
            scale_fill_manual(values = cols_scenarios),
          
          nrow = 2, common.legend = T)

# Figure S9 - scenario differences salt and tempat different flow levels -----
# uses the flow_cats and scenarios
ggarrange(forecasts |> ungroup() |> 
            filter(variable == 'temperature',
                   depth == 0.5,
                   datetime > as_date(reference_date)) |> 
            select(datetime, reference_date, variable, parameter, prediction, depth, model_id) |> 
            pivot_wider(names_from = model_id, values_from = prediction) |> 
            mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
                   `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest)  |> 
            select(-starts_with('glm')) |> 
            pivot_longer(cols = `raise barrages`:`lower barrages`,  names_to = 'scenario', values_to = "difference") |>
            mutate(horizon = as.numeric(datetime - as_date(reference_date)))  |> 
            full_join(flow_cats, by = 'reference_date') |> 
            reframe(.by = c('horizon', 'scenario', 'flow_category'),
                    quantile5 = quantile(difference, 0.05, na.rm = T),
                    quantile95 = quantile(difference, 0.95, na.rm = T),
                    median = median(difference, na.rm = T))|> 
            ggplot(aes(x=horizon)) +
            geom_hline(yintercept = 0)+
            geom_ribbon(aes(ymin = quantile5,ymax= quantile95, fill = scenario),
                        alpha = 0.1)  +
            geom_line(aes(y=median, colour = scenario))  +
            scale_y_continuous(expand = c(0.01,0)) +
            theme_bw() +
            labs(y= "Temperature difference from\nreference (°C)", x = 'Horizon (days)') +
            facet_wrap(~flow_category, labeller = labeller(flow_category = flow.labs))+
            scale_colour_manual(values = cols_scenarios) +
            scale_fill_manual(values = cols_scenarios), 
          
          forecasts |> ungroup() |> 
            filter(variable == 'salt',
                   depth == 0.5,
                   datetime > as_date(reference_date)) |> 
            select(datetime, reference_date, variable, parameter, prediction, depth, model_id) |> 
            pivot_wider(names_from = model_id, values_from = prediction) |> 
            mutate(`raise barrages` = glm_flare_v3_crest_up_0.1 - glm_flare_v3_crest,
                   `lower barrages` = glm_flare_v3_crest_down_0.1 - glm_flare_v3_crest)  |> 
            select(-starts_with('glm')) |> 
            pivot_longer(cols = `raise barrages`:`lower barrages`,  names_to = 'scenario', values_to = "difference") |>
            mutate(horizon = as.numeric(datetime - as_date(reference_date)))  |> 
            full_join(flow_cats, by = 'reference_date') |> 
            reframe(.by = c('horizon', 'scenario', 'flow_category'),
                    quantile5 = quantile(difference, 0.05, na.rm = T),
                    quantile95 = quantile(difference, 0.95, na.rm = T),
                    median = median(difference, na.rm = T))|> 
            ggplot(aes(x=horizon)) +
            geom_hline(yintercept = 0)+
            geom_ribbon(aes(ymin = quantile5,ymax= quantile95, fill = scenario),
                        alpha = 0.1)  +
            geom_line(aes(y=median, colour = scenario))  +
            scale_y_continuous(expand = c(0.01,0)) +
            theme_bw() +
            labs(y= "Salinity difference from\nreference (ppt)", x = 'Horizon (days)') +
            facet_wrap(~flow_category, labeller = labeller(flow_category = flow.labs))+
            scale_colour_manual(values = cols_scenarios) +
            scale_fill_manual(values = cols_scenarios),
          
          nrow = 2, common.legend = T)

# Figure S7 evaluation by inflow conditions ------------------
scores |>
  filter(variable %in% eval_vars,
         depth %in% eval_depths | is.na(depth),
         horizon > 0) |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime)),
         sq_error = (median - observation)^2) |> 
  full_join(flow_cats, by = 'reference_date') |> 
  reframe(.by = all_of(c('horizon', 'variable', 'depth', 'model_id', 'flow_category')),
          median_crps = median(crps, na.rm = T),
          rmse = sqrt(mean(sq_error, na.rm = T))) |>
  ggplot(aes(x=horizon, y=median_crps, colour = model_id)) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
  theme_bw() +
  facet_grid(variable~flow_category, 
             scales = 'free', labeller = labeller(variable = var.labs,
                                                  flow_category = flow.labs)) +
  labs(y = 'Median CRPS', x = "Horizon (days)") +
  scale_colour_manual(values = cols_mods, labels = mod.labs, breaks = names(mod.labs), name = 'Forecast model')  +
  theme(legend.position = 'top')


# Figure S1 modelled losses ----------------------------------
modelled_losses <- read_csv('R/helper_data/modelled_losses.csv')

modelled_losses |> 
  mutate(month = match(month, month.name)) |>  # reorder to water year
  pivot_longer(-month, names_to = 'flow_SAborder', names_prefix = 'GLd_',
               values_to = 'losses') |> 
  dplyr::mutate(days_in_month = lubridate::days_in_month(month),
                losses = (as.numeric(losses)/days_in_month) * 1000, # convert to ML/d from GL/m
                flow_SAborder = (as.numeric(flow_SAborder))*1000, # convert to ML/d from GL/d
                month = as.factor(month)) |> 
  mutate(month = factor(month(as.numeric(month), label = T),
                        levels = month.abb[c(7:12, 1:6)])) |>  # reorder to water year
  ggplot(aes(x = month,
             y=losses, colour = as_factor(flow_SAborder))) +
  geom_point() +
  scale_color_viridis_d(option  = 'magma', name = 'Flow at SA border (ML/day)', begin = 0.1, end = 0.9) +
  theme_bw() +
  labs(x = 'Month', y = 'Modelled losses (ML/day)')


# Figure S5 - method for calculating the scenario/inflow effects ---------------

minflow_param <- forecasts |> ungroup() |> 
  filter(variable == 'inflow') |> 
  mutate(prediction == ifelse(prediction == -9999, 0, prediction)) |> # means there is no flow
  reframe(.by = c('reference_date', 'parameter'),
          tot_flow = sum(prediction, na.rm = T)) |>
  group_by(reference_date) |> 
  slice_min(tot_flow) |> 
  mutate(flow_extreme = "min") |> 
  select(-tot_flow)


maxflow_param <- forecasts |> ungroup() |> 
  filter(variable == 'inflow') |> 
  mutate(prediction == ifelse(prediction == -9999, 0, prediction)) |> # means there is no flow
  reframe(.by = c('reference_date', 'parameter'),
          tot_flow = sum(prediction, na.rm = T)) |>
  group_by(reference_date) |>
  slice_max(tot_flow) |> 
  mutate(flow_extreme = "max") |> 
  select(-tot_flow)


flow_extreme_scenarios <- maxflow_param |> 
  full_join(minflow_param)

# to just the ensemble members and scenarios of interest
flow_extreme_fc <- forecasts |> 
  filter(variable %in% c('temperature', 'salt', 'depth'),
         scenario != 'reference', # take only the raise and lower
         datetime > as_date(reference_date),
         depth %in% c(NA, 0.5)) |> 
  inner_join(flow_extreme_scenarios, by = join_by(parameter, reference_date)) 



## Step 1 -------------------#
# identify ensemble members

step1 <- forecasts |> 
  filter(variable %in% c('inflow'),
         scenario == 'reference', 
         datetime > as_date(reference_date),
         depth %in% c(NA, 0.5)) |> 
  full_join(flow_extreme_scenarios, by = join_by(parameter, reference_date)) |> 
  filter(reference_date == example_ref_date) |> 
  mutate(flow_extreme = ifelse(is.na(flow_extreme), 'none', flow_extreme),
         alpha = ifelse(flow_extreme == 'none',0.6, 1)) |> 
  
  ggplot(aes(x = as.numeric(as_date(datetime) - as_date(reference_date)), 
             y = prediction/1000,
             group = parameter, colour = flow_extreme, alpha = alpha, lineype = flow_extreme)) + 
  geom_line() + 
  facet_wrap(~reference_date)  +
  scale_colour_manual(breaks = c('max', 'min'),labels = c('maximum', 'minimum'),
                      values = cols_inflowens, na.value = 'grey60', 
                      name = 'Inflow prediction') +
  scale_linetype_manual(breaks = c('max', 'min'), values = c('dashed', 'dotted'), na.value = 'solid',
                        name = 'Inflow prediction') +
  scale_alpha_continuous(guide=FALSE) +
  theme_bw() + 
  theme(legend.position = 'top') +
  labs(x = 'Horizon (days)', y = 'Inflow prediction (ML/day)') +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5))

## Step 2 -------------------#
# identify in-situ predictions
library(ggrepel)
labels <-  c('lower_barrages' = 'a',
             'raise_barrages' = 'b',
             'minimum' = 'c',
             'maximum' = 'd')
step2 <- 
  flow_extreme_fc |> 
  filter(variable == 'temperature', 
         reference_date == example_ref_date) |> 
  mutate(label = ifelse(as.numeric(as_date(datetime) - as_date(reference_date)) == 30,
                        case_when(scenario == "lower barrages" & flow_extreme == 'min' ~ "a",
                                  scenario == "lower barrages" & flow_extreme == 'max' ~ "c",
                                  scenario == "raise barrages" & flow_extreme == 'min' ~ "b",
                                  scenario == "raise barrages" & flow_extreme == 'max' ~ "d"),
                        NA)) |> 
  ggplot(aes(x = as.numeric(as_date(datetime) - as_date(reference_date)), 
             y = prediction)) + 
  geom_line(aes(colour = scenario, linetype = flow_extreme)) + 
  facet_wrap(~reference_date)  +
  scale_colour_manual(values = cols_scenarios, name = 'Outflow barrage\nscenario', 
                      labels = c('lower barrages', 'raise barrages')) +
  scale_linetype_manual(name = 'Inflow prediction', labels = c('maximum', 'minimum'),
                        values = c('dashed', 'solid')) +
  scale_alpha_continuous(guide = FALSE) +
  theme_bw()  + 
  geom_text_repel(aes(label = label), nudge_x = 0.3) +
  labs(x = 'Horizon (days)', y = 'Temperature prediction (°C)')


## Step 3 ----------------#

step3_labs <- c('min' = 'Scenario effect under minimum inflow (a-b)', 
                'max'= 'Scenario effect under maximum inflow (c-d)',
                "raise barrages" = "Inflow effect under raised barrages (b-d)",
                "lower barrages" = "Inflow effect under lower barrages (a-c)")
step3_cols <- c('min' = "#FCCE25FF",
                'max' = "#F1844BFF",
                "raise barrages" = "#0B0405FF", 
                "lower barrages" = "#357BA2FF" )
step3_lines <- c('min' = 'solid',
                 'max' = 'solid',
                 "raise barrages" = 'dashed', 
                 "lower barrages" = 'dashed')

# Calculate the differences
step3 <- bind_rows(flow_extreme_fc  |> 
                     filter(variable == 'temperature',
                            reference_date == example_ref_date) |> 
                     select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
                     pivot_wider(names_from = scenario, values_from = prediction) |> 
                     mutate(diff = abs(`lower barrages` - `raise barrages`),
                            group = flow_extreme),
                   
                   flow_extreme_fc |> 
                     filter(variable == 'temperature',
                            reference_date == example_ref_date) |> 
                     select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
                     pivot_wider(names_from = flow_extreme, values_from = prediction) |> 
                     mutate(diff = abs(min - max), 
                            group = scenario) ) |> 
  ggplot(aes(x = as.numeric(as_date(datetime) - as_date(reference_date)),
             y = diff, colour = group, linetype = group)) +
  geom_line() +
  facet_wrap(~reference_date) + 
  scale_colour_manual(values = step3_cols, 
                      name = '',
                      breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                      labels = step3_labs) +
  scale_linetype_manual(values = step3_lines, 
                        name = '',
                        breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                        labels = step3_labs) +
  theme_bw() +
  labs(y = 'Absolute difference in\nwater temperature (°C)', x = 'Horizon (days)') + 
  guides(colour=guide_legend(nrow=2, theme = theme(legend.key.spacing.x = unit(1, 'cm'))),
         linetype=guide_legend(nrow=2, theme = theme(legend.key.spacing.x = unit(1, 'cm'))))



## Step 4 ---------------------#
step4 <- bind_rows(flow_extreme_fc  |> 
                     filter(variable == 'temperature') |> 
                     select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
                     pivot_wider(names_from = scenario, values_from = prediction) |> 
                     mutate(diff = abs(`lower barrages` - `raise barrages`),
                            group = flow_extreme) |> 
                     reframe(.by = c(reference_date, group, variable), 
                             median_diff = median(diff)),
                   
                   flow_extreme_fc |> 
                     filter(variable == 'temperature') |> 
                     select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
                     pivot_wider(names_from = flow_extreme, values_from = prediction) |> 
                     mutate(diff = abs(min - max), 
                            group = scenario) |> 
                     reframe(.by = c(reference_date, group, variable), 
                             median_diff = median(diff))) |> 
  ggplot(aes(x=as_date(reference_date), y=median_diff, colour = group, linetype = group))+
  geom_line() +
  facet_wrap(~variable, scales = 'free', nrow = 3, labeller = labeller(variable = var.labs)) +
  scale_colour_manual(values = step3_cols, 
                      name = '',
                      breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                      labels = step3_labs) +
  scale_linetype_manual(values = step3_lines, 
                        name = '',
                        breaks = c('min', 'max', 'lower barrages', 'raise barrages'), 
                        labels = step3_labs) +
  theme_bw() +
  theme(legend.position = 'right') +
  labs(y = 'Absolute difference in\nwater temperature (°C)', x = 'Forecast generation date')

ggarrange(ggarrange(step1, step2, align = 'h', 
                    labels = c("A", "B"), 
                    hjust = c(-4, -3.5), vjust = 7.5,
                    font.label =  list(size = 18, color = "black", face = "bold", family = NULL)),
          ggarrange(step3, step4, align = 'h', common.legend = T, legend = 'bottom',
                    labels = c("C", "D"), 
                    hjust =  c(-4, -3.5), vjust = 3.5,
                    font.label =  list(size = 18, color = "black", face = "bold", family = NULL)), nrow = 2)

# Figure S11 - second example forecast --------------
example_fc_2 <- forecasts |> ungroup() |> 
  filter(reference_date %in% "2025-02-08",
         variable %in% 'depth',
         prediction != -9999) 


example_fc_2 |> 
  select(datetime, reference_date, variable, parameter, prediction, model_id, scenario) |> 
  mutate(prediction = ifelse(variable == 'depth', prediction - 5.3, prediction)) |> 
  reframe(.by = c("datetime", "variable", "scenario","reference_date"),
          median = median(prediction, na.rm = T),
          q97.5 = quantile(prediction, 0.975, na.rm = T),
          q02.5 = quantile(prediction, 0.025, na.rm = T)) |> 
  mutate(variable = factor(variable,
                           levels = c("crest_elev", "depth",
                                      "temperature", "salt",
                                      "overflow_flow", "salt_export"),
                           labels = c("A) barrage height (m, above AHD)", "B) lake level (m, above AHD)",
                                      "C) in-situ lake water temperature (C)", "D) in-situ lake salinity (ppt)",
                                      "E) barrage outflow from lake (ML/day)", "F) salt export from lake (tonnes/day)")),
         scenario = factor(scenario,
                           levels = c("reference", "raise barrages", "lower barrages"))) |> 
  mutate(median = ifelse(datetime < as_datetime(reference_date) & scenario != "reference", NA, median),
         q97.5 = ifelse(datetime < as_datetime(reference_date) & scenario != "reference", NA, q97.5),
         q02.5 = ifelse(datetime < as_datetime(reference_date) & scenario != "reference", NA, q02.5)) |> 
  ggplot(aes(x=datetime, y=median)) +
  geom_ribbon(aes(ymax = q97.5, ymin = q02.5, fill = scenario), alpha = 0.1) +
  geom_line(aes(y =q97.5, colour = scenario), linetype = 'dashed', alpha = 0.7) +
  geom_line(aes(y =q02.5, colour = scenario), linetype = 'dashed', alpha =0.7) +
  geom_line(aes(colour = scenario)) +
  facet_wrap(~reference_date) +
  geom_vline(aes(xintercept = as_datetime("2025-02-08")), linetype = 'dashed') +
  theme_bw() +
  labs(y="Median prediction with 95% predictive intervals", x = "Date") +
  scale_colour_manual(values = cols_scenarios) +
  scale_fill_manual(values = cols_scenarios)



# Figure S2 - Spin upperiod ------------
spinup_DA <- forecasts |>
  dplyr::filter(reference_date == ref_dates[1],
                scenario == 'reference',
                datetime < as_date(reference_date),
                variable %in% c('crest_elev', 'lw_factor'))

spinup_DA |> 
  ggplot(aes(x=datetime,y=prediction,group = parameter)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~variable, nrow = 2, scales = 'free') +
  theme_bw() +
  labs(y = 'parameter prediction') +
  scale_x_datetime(date_labels = '%d %b', name = 'Date')
# Figure S10 - logistic regression -----
# See the logistic_regression.R script for analysis and plots
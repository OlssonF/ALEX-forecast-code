# ALEX-forecast-code

This code reproduces figures from the Lake Alexandrina scenario forecasting using the FLARE (Forecasting Lake And Reservoir Ecosystems) system in the manuscript by Olsson et al. titled "Developing scenario-based, near-term iterative forecasts to inform water management" If you have any questions, contact Freya Olsson at freyao\@vt.edu

# Instructions to reproduce manuscript + SI figures:

1.  Download or clone github repository to your local computer

2.  Run `install_packages.R` in the `workflows/scenarios_reforecast` folder to download GLM and FLARE packages and their dependencies

3.  Run `download_forecasts_scores.R` in the `workflows/scenarios_reforecasts` folder to download forecasts and scores from Zenodo. 

4.  Run `04_ManuscriptPlots.R` script in the `workflows/scenarios_reforecasts` folder to reproduce manuscript and supplemental figures

# Instructions to reproduce FLARE forecasts and scores:

1.  Run `install_packages.R` in the `workflows/scenarios_reforecasts` folder to download the LER and FLARE packages and their dependencies

2.  Run `01_combined_workflow_scenarios.R` in the `workflows/scenarios_reforecasts` folder to iteratively generate the scenario forecasts from FLARE forecasts for every forecast date in the forecast period

    **Note** Running weekly forecasts for two years and all three scenarios will take \> 4-5 days depending on your computation.

3. Run `01_climatology_workflow.R` in the `workflows/scenarios_reforecasts` to generate the null climatology forecasts, score the forecasts, and write the forecasts and scores to a parquet database

4. Run `01_persistence_workflow.R` in the `workflows/scenarios_reforecasts` to generate the null persistence forecasts, score the forecasts, and write the forecasts and scores to a parquet database

5. Run `02_scenario_evaluation.R` in the `workflows/scenarios_reforecasts` to score the FLARE reference scenario forecast

6. Run `03_plots.R` script in the `workflows/scenarios_reforecasts` folder to reproduce manuscript and supplemental figures. To run the logistic regression analysis the `logistic regression.R` will need to be run.  

7. Run `04_ms_values.R` script in the `workflows/scenarios_reforecasts` folder to reproduce values that appear in the ms.
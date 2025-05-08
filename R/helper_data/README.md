This README details some of the data processing steps that occurred to generate the `helper_data` that is used in the inflow model generation. These data are derived from model output provided by the South Australia (SA) Department for Environment and Water (DEW) from their Source Hydrological model of the Murray River. Be sure to check the units!!

The datasets are:

1.  `modelled_losses.csv` - estimated losses from the SA border to Wellington (from abstraction, water use, and climate evaporation etc.) per month based on the entitlement flow at the SA border (in GL per day). The losses are a function of the flow and the month (seasonal differences).
2.  `travel_times.csv` - the estimated travel time in days based on the flow at the SA border (in ML per day). See details on its calculation below.
3.  `entitlement_flow.csv` - details the average entitlment flows to SA per month (in ML per day). The entitlement flow represents the amount of flow into SA under the Murray-Darling Basin Agreement for water allocation holders.
4.  `eflow.csv` - details the average environmental flows (eflows) to SA per month in (GL per month).

## Modelled losses:

Data were provided by DEW and comprise of the difference in flow at SA border and Wellington from a series Source model runs starting with Entitlement only (\~5 GL/d) up to 40 GL/d (regulated/within channel upper limit) in 5 GL/d steady state increments. Data are in GL/m.

## Travel times:

The travel time data are consolidated from a travel time/flow piecewise relationships from Source model (summed reaches from SA border to Wellington). Raw data provided by DEW were interpolated to consistent flow levels (approximately 5000ML/day increments) and then summed (e.g. border-Lock6 + Lock6-Lock5 + Lock5-Lock + ... + Lock1-Wellington). Data described in this report and provided by DEW <https://www.waterconnect.sa.gov.au/Content/Publications/DEW/TechnicalReport_RefinementsToTheRiverMurraySourceModelInSA_2020_final.pdf>

## Entitlement and environmental flows:

Entitlement flow was provided by DEW as average flow to SA per month, which is then disaggregated to a daily time step. Monthly environmental flow to SA was derived from environmental water accounting between July 2012 and June 2023 at a monthly time step, showing the contribution of environmental water to flow at the SA border.

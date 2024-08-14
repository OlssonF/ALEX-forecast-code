#' @title Interpolate observations to a continuous time series
#' @details Generating an interpolated target that can be used as a driver (could be used as inflow/outflow or other datafrmaes in the targets format)
#' @param targets name of the target file csv
#' @param lake_directory FLARE working directory eg. ~home/rqthomas/FCRE-forecast-code
#' @param site_id code for site being forecasted
#' @param variables optional vector of variables to generate the interpolation for. Default is no filtering (all variables included from targets)
#' @param depth logical, is depth a column in these targets 
#' @return dataframe of interpolated time series
#' @export

interpolate_targets <- function(targets, 
                                lake_directory,
                                targets_dir = 'targets',
                                site_id,
                                variables = NULL,
                                depth = T,
                                method = 'linear') {
  # read in data
  df <- readr::read_csv(file.path(lake_directory, targets_dir, site_id, targets),
                        show_col_types = F)
  
  # which variables are we using?
  if (is.null(variables)) {
    filter_vars  <- dplyr::distinct(df,variable) |> 
      pull(variable)
  } else {
    filter_vars <- variables
  }
  
  # is depth a column in these targets?
  if (depth) {
    grouping_vars <- c('depth', 'variable')
  }else {
    grouping_vars <- 'variable'
  }
  
  # generate an interpolation
  df_interp <- df |>
    dplyr::filter(variable %in% filter_vars) |> 
    dplyr::group_by(dplyr::pick(any_of(grouping_vars))) |> 
    dplyr::arrange(dplyr::pick(any_of(grouping_vars), datetime)) |> 
    dplyr::mutate(observation = imputeTS::na_interpolation(observation,option = method)) |> 
    dplyr::ungroup()
  
  return(df_interp)
  
}

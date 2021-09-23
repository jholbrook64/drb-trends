## ---------------------------
##
## Script name: 1_fetch.R
##
## Purpose of script: mapping reaches $ their annual observations
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes: targets file for data fetches
##     ~
## ---------------------------

source("1_fetch/src/data_selection.R")
source("1_fetch/src/data_summary.R")

tar_option_set(packages = c('tidyverse'))

fetch_targest_list <- list(

  # select site-months for analysis
  tar_target(monthly_data,
             filter_monthly_data('1_fetch/in/raw_drb_temp_data.rds')),

  # select site-years for analysis
  tar_target(annual_data,
             filter_annual_data('1_fetch/in/raw_drb_temp_data.rds')),

  # reduce down to monthly data
  tar_target(mean_monthly_data,
             group_time(monthly_data)),

  # reduce down to annual data
  tar_target(mean_annual_data,
             group_year(annual_data)),

  #tar_target(tiles_month_site,
   #          tile_plot_func(mean_monthly_data), format = 'file'),

  # group data to prepare for mapping
  tar_group_by(mean_monthly_data_g,
               mean_monthly_data, site_id, series, month), # previoulsy, seg_id_nat

  tar_group_by(mean_annual_data_g,
               mean_annual_data, site_id, series),

  # monthly regressions
  meanofmean_regressions <- tar_target(meanofmean_regression,
             flexible_linear_regression(mean_monthly_data_g, 1), pattern = map(mean_monthly_data_g), iteration = "group"),

  meanofmax_regressions <- tar_target(meanofmax_regression,
             flexible_linear_regression(mean_monthly_data_g, 2), pattern = map(mean_monthly_data_g), iteration = "group"),

  meanofmin_regressions <- tar_target(meanofmin_regression,

             flexible_linear_regression(mean_monthly_data_g, 3), pattern = map(mean_monthly_data_g), iteration = "group"),

  annual_regressions <- tar_target(annual_regression,
                                   flexible_linear_regression(mean_annual_data_g, 4), pattern = map(mean_annual_data_g), iteration = "group"),

  # combine targets
  tar_combine(regress_data_monthMeans, meanofmean_regressions),
  tar_combine(regress_data_monthMaxs, meanofmax_regressions),
  tar_combine(regress_data_monthMins, meanofmin_regressions)
  #tar_combine(regress_data_annual, annual_regressions)

)


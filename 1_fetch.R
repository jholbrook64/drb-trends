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
  # tar_target(clean_Monthly,
  #            clean_monthly('1_fetch/in/obs_temp_drb.rds')),
  
  tar_target(clean_Monthly,
             clean_monthly('1_fetch/in/raw_drb_temp_data.rds')),
  
  tar_target(select_data,
             filter_data(clean_Monthly)),
  
  tar_target(data_for_trend_analysis,                            
             group_time(select_data)),
  
  tar_target(NA_removed_data_for_trend_analysis,
             clean_monthly_mean(data_for_trend_analysis)),
  
  
  tar_target(year_trend_analysis,
             group_year(select_data)),
  
  tar_target(tiles_month_site,
             tile_plot_func(data_for_trend_analysis), format = 'file'),
  
  # special cleaning target for the mean of mean regressions - updated on 
  tar_target(clean_all_month_mean,
             drop_na(data_for_trend_analysis))
  )
  # end data cleaning targets
  
summarize_targets_list <- list(
  
  tar_group_by(month_data,
               data_for_trend_analysis, month, site_id), # previoulsy, seg_id_nat
  
  # special consideration since this one has all NA regressands in a branch. 
  tar_group_by(mean_month,
               clean_all_month_mean, month, site_id),
  
  tar_group_by(year_data,
               year_trend_analysis, site_id), # previoulsy, seg_id_nat
  
  # tar_group_select(mean_month, filter(na.rm(month_data$month_mean), by = NULL)),
  # 
  # tar_group_select(max_month, filter(na.rm(month_data$month_meanOfMax), by = NULL)),
  # 
  # tar_group_select(min_month, filter(na.rm(month_data$month_meanOfMin), by = NULL)),
                                      
                                      # change the data param on line 51 pleases, 9-1-2021
  meanofmean_regressions <- tar_target(meanofmean_regression,
                                       flexible_linear_regression(mean_month, 1), pattern = map(mean_month), iteration = "group"),
  
  meanofmax_regressions <- tar_target(meanofmax_regression,
                                      flexible_linear_regression(month_data, 2), pattern = map(month_data), iteration = "group"),
  
  meanofmin_regressions <- tar_target(meanofmin_regression,
                                      flexible_linear_regression(month_data, 3), pattern = map(month_data), iteration = "group"),
  
  Annual_regressions <- tar_target(Annual_regression,
                                  flexible_linear_regression(year_data, 4), pattern = map(year_data), iteration = "group")

  )
  # end branched regressions targets
  
  combine_targets_list <- list(
  
  combined_monthMean <- tar_combine(regress_data_monthMeans, meanofmean_regressions),
  
  combined_monthMax <- tar_combine(regress_data_monthMaxs, meanofmax_regressions),
  
  combined_monthMin <- tar_combine(regress_data_monthMins, meanofmin_regressions)#,

  #combined_annual <- tar_combine(regress_data_annual, Annual_regressions)
)
  # end combined targets list


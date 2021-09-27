## ---------------------------
##
## Script name: 2_map.R
##
## Purpose of script: mapping reaches $ their annual observations
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes: targets file for creating visualizations
##     ~
## ---------------------------

source("2_map/src/data_map.R")

tar_option_set(packages = c('tidyverse', 'sf', 'mapview'))

map_targets_list <- list(
  
  tar_target(boxplots_month_max, 
             boxplot_func(meanofmax_regression, 1, "2_map/in/reservoir_coding.csv"), format = 'file'),
  
  tar_target(boxplots_month_mean, 
             boxplot_func(meanofmean_regression, 2, "2_map/in/reservoir_coding.csv"), format = 'file'),
  
  tar_target(boxplots_month_min, 
             boxplot_func(meanofmin_regression, 3, "2_map/in/reservoir_coding.csv"), format = 'file'),
  
  tar_group_by(months,
                meanofmean_regression, Month),

  tar_target(site_map_pngs,
             map_sites(months, '1_fetch/in/network.rds', '1_fetch/in/crosswalk_site_reach.rds'), format = 'file', pattern = map(months)),

  tar_target(satellite_image_drbTrends,
             map_tiles(meanofmean_regression, '1_fetch/in/network.rds', '1_fetch/in/crosswalk_site_reach.rds'), format = 'file')
  )

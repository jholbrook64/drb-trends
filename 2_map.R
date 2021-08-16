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

tar_option_set(packages = c('tidyverse', 'sf'))

map_targets_list <- list(
  
  tar_target(boxplots_year, 
             boxplot_func(year_data, 1), format = 'file'),
  
  tar_target(boxplots_month, 
             boxplot_func(month_data, 2), format = 'file'),
  
  tar_group_by(months,
               meanofmean_regression, Month),

  # this is to create 12 plots that show the difference in the slope, represented as "year" for the regressions over each branch. 
  tar_target(site_map_pngs,
             map_sites(months, '1_fetch/in/network.rds'), format = 'file', pattern = map(months))

  )

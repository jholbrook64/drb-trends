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
             boxplot_func(Annual_regression, 1), format = 'file'),
  
  tar_target(boxplots_month, 
             boxplot_func(meanofmean_regression, 2), format = 'file'),
  
  tar_target(hist_year, 
             hist_func(Annual_regression), format = 'file'),
  
 # 8-17-2021 the reason why this group by is needed is because 
 # patterns can only branch over explicitly declared targets in the pipeline
 # otherwise, the attribute to branch over is hidden from targets
  tar_group_by(months,
                meanofmean_regression, Month),

  # this is to create 12 plots that show the difference in the slope, represented as "year" for the regressions over each branch. 
  tar_target(site_map_pngs,
             map_sites(months, '1_fetch/in/network.rds'), format = 'file', pattern = map(months))

  )

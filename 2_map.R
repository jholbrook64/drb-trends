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
  
  tar_target(hist_year, 
             hist_func(meanofmax_regression), format = 'file'), #1
  
 # 8-17-2021 the reason why this group by is needed is because 
 # patterns can only branch over explicitly declared targets in the pipeline
 # otherwise, the attribute to branch over is hidden from targets
  tar_group_by(months,
                meanofmean_regression, Month),

  # this is to create 12 plots that show the difference in the slope, represented as "year" for the regressions over each branch. 
  tar_target(site_map_pngs,
             map_sites(months, '1_fetch/in/network.rds', '1_fetch/in/crosswalk_site_reach.rds'), format = 'file', pattern = map(months))

  # tar_target(satellite_image_drbTrends,
  #            map_tiles(meanofmean_regression, '1_fetch/in/network.rds'), format = 'file')
  )

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
  # this may be needed in the construction of 1 map per each month
  # tar_files(files, c("January.png", "February.png", "March.png", "April.png", "May.png", "June.png", "July.png", "August.png",
  #                    "September.png", "October.png", "November.png", "December.png")),
  
  # this is to create 12 plots that show the difference in the slope, represented as "year" for the regressions over each branch. 
  tar_group_by(months,
               meanofmean_regression, Month),

  tar_target(site_map_pngs,
             map_sites(months, '1_fetch/in/network.rds'), format = 'file', pattern = map(months))

  )
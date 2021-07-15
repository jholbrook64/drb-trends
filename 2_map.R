# targets file for mapping data
source("2_map/src/data_map.R")

tar_option_set(packages = c('tidyverse', 'sf'))

map_targets_list <- list(
  # this may be needed in the construction of 1 map per each month
  
  # tar_files(files, c("January.png", "February.png", "March.png", "April.png", "May.png", "June.png", "July.png", "August.png", 
  #                    "September.png", "October.png", "November.png", "December.png")),
  
  tar_target(site_map_pngs,
             map_sites(data_for_trend_analysis, '1_fetch/in/network.rds'), format = 'file')
)
source('src/site_selection_code.R')

tar_option_set(packages = c('sf', 'tidyverse', 'mapview'))

p2_targets_list <- list(  
  tar_target(annual_table,
            meta_data(annual_summary), format = 'file'),
  
  tar_target(time_grouped,
             group_time(add_days))
  )
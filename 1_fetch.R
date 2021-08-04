source('src/time_summaries.R')

tar_option_set(packages = c('sf', 'tidyverse', 'mapview'))

p1_targets_list <- list(
  tar_target(annual_summary, 
               evaluate_annual('in/obs_temp_drb.rds')),
  
  tar_target(monthly_summary,
             evaluate_monthly('in/obs_temp_drb.rds')), #, format = 'file'
  
  tar_target(clean_Monthly,
             clean_monthly('in/obs_temp_drb.rds')),
  
  tar_target(add_days,                            # take return of the previous target
             add_days_in_month(clean_Monthly)) #, format = 'file'

  # add_days goes straight to the function group_time, which return target time_grouped
  )
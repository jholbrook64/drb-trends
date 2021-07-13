source('src/site_selection_code.R')

tar_option_set(packages = c('sf', 'tidyverse', 'mapview'))

p2_targets_list <- list(  
  tar_target(annual_table,
            meta_data(annual_summary), format = 'file'),
  
  tar_target(time_grouped,
             group_time(add_days)),
  
  tar_target(trend_analysis_grouped_by_months,
             monthly_trend_analysis(time_grouped)),
  
  tar_target(all_sites_regression,
             DRB_regression_monthly(trend_analysis_grouped_by_months, fileout = '2_process/out/DRB_regress_Values.pdf'),
             format = "file")
  
  # tar_target(each_site_regression,
  #            regression_for_each_site_monthly(trend_analysis_grouped_by_months, fileout = '2_process/out/DRB_site_regress_Values.csv'),
  #            format = "file")
  )
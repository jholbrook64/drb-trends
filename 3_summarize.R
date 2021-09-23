## ---------------------------
##
## Script name: 3_summarize.R
##
## Purpose of script: mapping reaches $ their annual observations
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes: targets file for data summaries
##     ~
## ---------------------------

source("3_summarize/src/summarize.R")
source("1_fetch/src/data_summary.R")

tar_option_set(packages = c('tidyverse'))

meta_summaries <- list(
  tar_target(density_plots,
             overlap_density(bind_regressions), format = 'file'),
  
  # tar_target(MonthMean_summary, summarize_table(regress_data_monthMeans), na.rm = ),
  # 
  # tar_target(MonthMax_summary, summarize_table(regress_data_monthMaxs)),
  # 
  # tar_target(MonthMin_summary, summarize_table(regress_data_monthMins)),
  
  #tar_target(Annual_summary, summarize_table(regress_data_annual)),
  
  # descriptive stats - create a summary of mins across annual branches:
  tar_target(descrip_annual, 
             summary(mean_annual_data_g$annual_mean),  pattern = map(mean_annual_data_g), iteration = "group"),
  
  
  # this simply load the highest trend target, I want to inlcudee this in my analysis
  # tar_target(look_at_this_segment3, month_data %>%
  #              filter(site_id == 'USGS-01434000') %>%
  #              filter(month == 7)),
  
  tar_target(The_line_plot, line_plot2(mean_annual_data_g), format = 'file')
  
  
)
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

#source("3_summarize/src/data_analysis.R")
source("1_fetch/src/data_summary.R")

tar_option_set(packages = c('tidyverse'))

meta_summaries <- list(
  
  tar_target(MonthMean_summary, summarize_table(regress_data_monthMeans)),
  
  tar_target(MonthMax_summary, summarize_table(regress_data_monthMaxs)),
  
  tar_target(MonthMin_summary, summarize_table(regress_data_monthMins)),
  
  #tar_target(Annual_summary, summarize_table(regress_data_annual)),
  
  # descriptive stats - create a summary of mins across annual branches:
  tar_target(descrip_annual, 
             summary(year_data$annual_mean),  pattern = map(year_data), iteration = "group"),
  
  
  # this simply load the highest trend target, I want to inlcudee this in my analysis
  tar_target(look_at_this_segment3, month_data %>%
               filter(site_id == 'USGS-01434000') %>%
               filter(month == 7)),
  
  tar_target(line_plot2, line_plot2(look_at_this_segment3), format = 'file')

  
)
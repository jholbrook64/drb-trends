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

source("3_summarize/src/data_analysis.R")

tar_option_set(packages = c('tidyverse'))

meta_summaries <- list(
  
  tar_target(MonthMean_summary, summarize_table(regress_data_monthMeans)),
  
  tar_target(MonthMax_summary, summarize_table(regress_data_monthMaxs)),
  
  tar_target(MonthMin_summary, summarize_table(regress_data_monthMins)),
  
  tar_target(Annual_summary, summarize_table(regress_data_annual))
  
  # other method of making targets list - I believe this is incorrect
  # regression_table <- list(regress_data_monthMeans, regress_data_monthMaxs, regress_data_monthMins,
  #                           regress_data_annual)
  
  # regression_table <- list(combined_monthMean, combined_monthMax, combined_monthMin,
  #                         combined_annual)
  
  # this should be the loop-through function that meakes a meta-syummary for each dataframe
  # for (i in length(regression_table)) {
  #   tar_target(table_summary, summarize_table(regression_table[i]))
  # }
  # 
  
)
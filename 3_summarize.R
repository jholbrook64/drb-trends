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
  
  tar_target(Annual_summary, summarize_table(regress_data_annual)),
  
  # descriptive stats - create a summary of mins across annual branches:
  tar_target(descrip_annual, 
             summary(year_data$annual_mean),  pattern = map(year_data), iteration = "group"),
  
  
  # this simply load the highest trend target, I want to inlcudee this in my analysis
  tar_target(look_at_this_segment3, month_data %>%
               filter(site_id == 'USGS-01434000') %>%
               filter(month == 1))
  # tar_target(look_at_this_segment4, month_data %>%
  #              filter(seg_id_nat == 1573) %>%
  #              filter(month == 5)),
  # tar_target(look_at_this_segment5, month_data %>%
  #              filter(seg_id_nat == 1568) %>%
  #              filter(month == 10))
              # min
  
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
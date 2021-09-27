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
  
  tar_target(regress_data_monthMeans_clean, drop_na(regress_data_monthMeans)),
  
  tar_target(regress_data_monthMaxs_clean, drop_na(regress_data_monthMaxs)),
  
  tar_target(regress_data_monthMins_clean, drop_na(regress_data_monthMins)),
  
  tar_target(MonthMean_summary, summarize_table(regress_data_monthMeans_clean)),
  
  tar_target(MonthMax_summary, summarize_table(regress_data_monthMaxs_clean)),
  
  tar_target(MonthMin_summary, summarize_table(regress_data_monthMins_clean)),

  
  # descriptive stats - create a summary of means across annual branches:
  tar_target(descrip_annual, 
             summary(mean_annual_data_g$annual_mean),  pattern = map(mean_annual_data_g), iteration = "group"),
  
  tar_target(study_site,
             filter(mean_monthly_data, site_id == "USGS-01425000")),
  
  tar_target(Site_line_plot, line_plot_function(study_site), format = 'file')
  
  
)
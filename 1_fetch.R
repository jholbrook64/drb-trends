# targets file for fetching data
source("1_fetch/src/data_selection.R")
source("1_fetch/src/data_summary.R")

tar_option_set(packages = c('tidyverse'))

# this will be a vector of every unque combo of site and month!
# unique_files_month_and_site <- function(month_vector, site_vector)
#   {
#   expand.grid(month_vector, site_vector)
#   }

fetch_targest_list <- list(
  tar_target(clean_Monthly,
             clean_monthly('1_fetch/in/obs_temp_drb.rds')),
  
  tar_target(data_for_trend_analysis,                            
             group_time(clean_Monthly))
)
  # end functions from "1_fetch/src/data_selection"
  
  # use branching here to subset rows:
summarize_targets_list <- list(
  
  tar_group_by(unique_data,
               data_for_trend_analysis, month, seg_id_nat),
  
  tar_target(sites, unique_data, pattern = map(unique_data)),
  
  # tar_target(regress_sites,
  #             regress_site(sites)),
  
  # tar_target(regress_each_site,
  #            regress_site(unique_data))
  
  # I've seen both example where an object assigned and where an object isn't assigned (has either s or no s)
  meanofmean_regressions <- tar_target(meanofmean_regression,
                                       flexible_linear_regression(sites, 1), pattern = map(sites)),
  
  meanofmax_regressions <- tar_target(meanofmax_regression,
                                      flexible_linear_regression(sites, 2), pattern = map(sites)),
  
  meanofmin_regressions <- tar_target(meanofmin_regression,
                                      flexible_linear_regression(sites, 3), pattern = map(sites)),
  
  Annual_regressions <- tar_target(Annual_regression,
                                   flexible_linear_regression(sites, 4), pattern = map(sites)),
  
  # branched_regressions <- tar_target(regress_value_vector,
  #             build_statistics(each_site_regression)),
  
  combined_monthMean <- tar_combine(regress_data_monthMeans, meanofmean_regressions),
  
  combined_monthMax <- tar_combine(regress_data_monthMaxs, meanofmax_regressions),
  
  combined_monthMin <- tar_combine(regress_data_monthMins, meanofmin_regressions),
  
  combined_annual <- tar_combine(regress_data_annual, Annual_regressions)
  
  # below are targets for data meta-summaries:
  
  # tar_target(df_positive, write_summary_positive(regress_data)),
  # 
  # tar_target(df_negative, write_summary_negative(regress_data)),
  # 
  # tar_target(df_final, bind_transposed(df_positive, df_negative))
)


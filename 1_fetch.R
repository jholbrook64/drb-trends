# targets file for fetching data
source("1_fetch/src/data_selection.R")
source("1_fetch/src/data_summary.R")

tar_option_set(packages = c('tidyverse'))

# this will be a vector of every unque combo of site and month!
unique_files_month_and_site <- function(month_vector, site_vector)
  {
  expand.grid(month_vector, site_vector)
  }

fetch_targest_list <- list(
  tar_target(clean_Monthly,
             clean_monthly('1_fetch/in/obs_temp_drb.rds')),
  
  tar_target(data_for_trend_analysis,                            
             group_time(clean_Monthly))
)
  # end functions from "1_fetch/src/data_selection"
  
  # use branching here to subset rows:
summarize_targets_list <- list(
  tar_target(month_vector,
             order(unique(data_for_trend_analysis$month))),
  
  tar_target(site_vector,
            unique(data_for_trend_analysis$seg_id_nat)),
  
  tar_group_by(unique_data,
               data_for_trend_analysis, month, seg_id_nat),
  
  tar_target(sites, unique_data, pattern = map(unique_data)),
  
  tar_target(regress_sites,
              regress_site(sites))
  
  # tar_target(each_site_regression,
  #             each_site_monthly(files), pattern = map(files)),
)


library(targets)

# source scripts that hold your functions
source('src/site_selection_code.R')
source('src/map_code.R')
source('src/time_summaries.R')
source('src/make_plots.R')

#install.packages("mapview")

# Set target-specific options such as packages.
tar_option_set(packages = c('sf', 'tidyverse', 'mapview'))

# End this file with a list of target objects.
list(
  # example of how you could read in all temperature data
  # and summarize annual data availability by river reach (seg_id_nat)
  tar_target(annual_summary, 
             evaluate_annual('in/obs_temp_drb.rds')),
  
  # example of how you could select some sites and map them using
  # all river segments from our modeling framework
  tar_target(site_map_png,
             map_sites(annual_summary, 'in/network.rds'), format = 'file'),
  
  
  # jack created:
  
  tar_target(annual_table,
            meta_data(annual_summary), format = 'file'),
  
  # tar_target(no_unique,
  #            write_up(annual_summary)),
  
  tar_target(monthly_summary,
             evaluate_monthly('in/obs_temp_drb.rds'), format = 'file'),
  
  tar_target(data_with_months,
             clean_monthly('in/obs_temp_drb.rds')),
  
  # tar_target(group_time,
  #            group_time(data_with_months), format = 'file'),
  
  tar_target(plot1, 
             plot_tot(in_monthDat = "out/monthly_Table.csv", fileout = "out/figure_1.png"), format = 'file'),
              
  tar_target(site_map_interactive,
             map_tiles(annual_summary, 'in/network.rds'), format = 'file')
)

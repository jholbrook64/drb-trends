source('src/map_code.R')
source('src/make_plots.R')

tar_option_set(packages = c('sf', 'tidyverse', 'mapview'))

p3_targets_list <- list(
  tar_target(site_map_png,
             map_sites(annual_summary, 'in/network.rds'), format = 'file'),
  
  tar_target(fig_text,
             plot_fig('in/obs_temp_drb.rds', add_days), format = 'file'),
  
  tar_target(All_Monthly_Sites_Plot, 
             plot_tot(add_days, fileout = "out/All_Monthly_Sites_Records.png"), format = 'file'),
  
  tar_target(Current_Monthly_Sites_Plot, 
             plot_current(add_days, fileout = "out/Current_Monthly_Sites_Records.png"), format = 'file'),
  
  tar_target(site_map_interactive,
             map_tiles(annual_summary, 'in/network.rds'), format = 'file'),
  
  tar_target(All_segements_plotted,
             plot_all_segments(time_grouped, fileout = "3_visualize/out/plots_page.pdf"), format = "file")
)
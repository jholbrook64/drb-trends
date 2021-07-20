## ---------------------------
##
## Script name: data_map.R
##
## Purpose of script: mapping reaches $ their annual observations
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~
## ---------------------------

map_sites <- function(data_for_trend_analysis_month, in_network) {
  
  net <- readRDS(in_network)[[1]]
  
  # join the network with the data so you can use things like
  # the number of observations for plotting
  
  net_d <- left_join(net, distinct(select(data_for_trend_analysis_month, seg_id_nat, month_meanOfMax)))

    #  assigns file name based off date month
  month <- unique(format(as.Date(data_for_trend_analysis_month$date)))
  out_filename <- paste("2_map/out/", month, ".png", sep = "")
  
  # look at all sites with data
  p <- ggplot(net_d) +
    geom_sf(color = 'gray') +
    # filter to sites with a n_all_time value, which indicates at least some data
    # color by how much data a segment has
    geom_sf(data = filter(net_d, !is.na(month_meanOfMax)), aes(color = month_meanOfMax)) +
    # change color scale
    scale_color_viridis_c(direction = -1, option = 'plasma', end = 0.95) +
    theme_bw()
  
  ggsave(out_filename, p, height = 7, width = 5)
  return(out_filename)
  
}
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
  # read the in_network piece
  net <- readRDS(in_network)[[1]]
  
  # join the network with the data so you can use things like
  # the number of observations for plotting
  
  net_d <- left_join(net, select(data_for_trend_analysis_month, seg_id_nat = seg_id_nat, Slope))
  
  names(net_d)[names(net_d) == 'Slope'] <- 'Warming_Trend_degC_Year'
  
  #colnames(net_d$Slope) <- "Warming_Trend_degC_Year"
  #colnames(net_d)[12] <- "Warming_trend_degC_Year"
  
  #  assigns file name based off date month, is same value for each branch
  month <- unique((data_for_trend_analysis_month$Month))
  month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                     "September", "October", "November", "December")
  monthname <- month_list[month]
  title <- "DRB wide warming trend for the month of "
  # device` must be NULL, a string or a function.
  out_filename <- paste("2_map/out/", month, sep = "")
  out_filename_ext <- paste0(substitute(out_filename))
  
  # look at all sites with data
  p <- ggplot(net_d) +
    geom_sf(color = 'gray') +
    # filter to sites with a n_all_time value, which indicates at least some data
    # color by how much data a segment has
    geom_sf(data = filter(net_d, !is.na(Warming_Trend_degC_Year)), aes(color = Warming_Trend_degC_Year)) +
    #geom_label(data = seg_id_nat, )
    scale_color_viridis_c(direction = -1, option = 'plasma', end = 0.95) +
    theme_bw() +
    ggtitle(paste(title, monthname, sep = ""))+ 
    xlab(expression(paste(Longitude^o,~'N'))) +
    ylab(expression(paste(Latitude^o,~'W')))
  #print("got here")
  #saving file
  ggsave(filename = month, p, device = "png", path = "2_map/out/", height = 7, width = 5)
  return(out_filename_ext)
}